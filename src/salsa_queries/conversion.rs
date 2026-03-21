use std::collections::HashMap;
use std::sync::Arc;

use crate::index::{FieldSummary, IndexView, MethodSummary};
use crate::language::java::JavaContextExtractor;
use crate::language::java::type_ctx::SourceTypeCtx;
use crate::language::java::{flow, members};
use crate::salsa_db::SourceFile;
use crate::salsa_queries::Db;
use crate::salsa_queries::{
    CompletionContextData, CursorLocationData, ExpectedTypeSourceData, FunctionalExprShapeData,
    FunctionalTargetHintData, MethodRefQualifierKindData, StatementLabelData,
    StatementLabelTargetKindData,
};
use crate::semantic::context::{
    CurrentClassMember, ExpectedTypeSource, FunctionalExprShape, FunctionalMethodCallHint,
    FunctionalTargetHint, MethodRefQualifierKind, StatementLabel, StatementLabelTargetKind,
};
use crate::semantic::types::type_name::TypeName;
use crate::semantic::{CursorLocation, LocalVar, SemanticContext};
use crate::workspace::AnalysisContext;
use tree_sitter_utils::traversal::{ancestor_of_kind, find_node_by_offset};

#[derive(Clone)]
pub struct RequestAnalysisState {
    pub analysis: AnalysisContext,
    pub view: IndexView,
}

/// Conversion layer between Salsa-compatible data and rich semantic types.
pub trait FromSalsaData<T> {
    fn from_salsa_data(
        data: T,
        db: &dyn Db,
        file: SourceFile,
        workspace: Option<&crate::workspace::Workspace>,
    ) -> Self;
}

pub trait FromSalsaDataWithAnalysis<T> {
    fn from_salsa_data_with_analysis(
        data: T,
        db: &dyn Db,
        file: SourceFile,
        workspace: Option<&crate::workspace::Workspace>,
        analysis: Option<&RequestAnalysisState>,
    ) -> Self;
}

impl FromSalsaData<CompletionContextData> for SemanticContext {
    fn from_salsa_data(
        data: CompletionContextData,
        db: &dyn Db,
        file: SourceFile,
        workspace: Option<&crate::workspace::Workspace>,
    ) -> Self {
        <Self as FromSalsaDataWithAnalysis<CompletionContextData>>::from_salsa_data_with_analysis(
            data, db, file, workspace, None,
        )
    }
}

impl FromSalsaDataWithAnalysis<CompletionContextData> for SemanticContext {
    fn from_salsa_data_with_analysis(
        data: CompletionContextData,
        db: &dyn Db,
        file: SourceFile,
        workspace: Option<&crate::workspace::Workspace>,
        analysis: Option<&RequestAnalysisState>,
    ) -> Self {
        let location = convert_cursor_location(&data.location);
        let imports = crate::salsa_queries::extract_imports(db, file);
        let existing_imports: Vec<Arc<str>> = imports.iter().cloned().collect();

        let mut ctx = SemanticContext::new(
            location,
            data.query.as_ref(),
            vec![],
            data.enclosing_class.clone(),
            data.enclosing_internal_name.clone(),
            data.enclosing_package.clone(),
            existing_imports.clone(),
        )
        .with_file_uri(data.file_uri.clone())
        .with_language_id(crate::language::LanguageId::new(data.language_id.clone()));

        if data.language_id.as_ref() == "java" {
            ctx = enrich_java_semantic_context(
                ctx,
                db,
                file,
                workspace,
                &data,
                existing_imports,
                analysis,
            );
        }

        ctx
    }
}

fn enrich_java_semantic_context(
    ctx: SemanticContext,
    db: &dyn Db,
    file: SourceFile,
    workspace: Option<&crate::workspace::Workspace>,
    data: &CompletionContextData,
    existing_imports: Vec<Arc<str>>,
    analysis: Option<&RequestAnalysisState>,
) -> SemanticContext {
    let source = file.content(db);
    let rope = ropey::Rope::from_str(source);
    let tree = crate::salsa_queries::parse::parse_tree(db, file)
        .expect("java completion conversion parse tree");
    let root = tree.root_node();
    let extractor = JavaContextExtractor::with_rope(
        Arc::<str>::from(source.as_str()),
        data.cursor_offset.min(source.len()),
        rope,
        None,
    );
    let cursor_node = extractor.find_cursor_node(root);

    let members = workspace
        .map(|ws| fetch_class_members_from_workspace(db, file, ws, data.cursor_offset))
        .unwrap_or_default();

    let method_map: HashMap<Arc<str>, Arc<MethodSummary>> = members
        .values()
        .filter_map(|member| match member {
            CurrentClassMember::Method(method) => {
                Some((Arc::clone(&method.name), Arc::clone(method)))
            }
            CurrentClassMember::Field(_) => None,
        })
        .collect();

    let type_ctx = if let Some(request_analysis) = analysis {
        SourceTypeCtx::from_view(
            data.enclosing_package.clone(),
            existing_imports.clone(),
            request_analysis.view.clone(),
        )
    } else {
        SourceTypeCtx::from_overview(
            data.enclosing_package.clone(),
            existing_imports.clone(),
            None,
        )
    };
    let type_ctx = Arc::new(type_ctx.with_current_class_methods(method_map));

    let local_variables = workspace
        .map(|ws| fetch_locals_from_workspace(db, file, ws, &data))
        .unwrap_or_else(|| {
            crate::salsa_queries::extract_visible_method_locals_from_source(
                source,
                data.cursor_offset,
                Some(&type_ctx),
            )
        });

    let static_imports = fetch_static_imports(db, file);
    let enclosing_class_member = cursor_node
        .and_then(|n| ancestor_of_kind(n, "method_declaration"))
        .or_else(|| find_node_by_offset(root, "method_declaration", data.cursor_offset))
        .or_else(|| {
            crate::language::java::utils::find_enclosing_method_in_error(root, data.cursor_offset)
        })
        .and_then(|m| members::parse_method_node(&extractor, &type_ctx, m));
    let active_lambda_param_names = workspace
        .map(|_| {
            crate::salsa_queries::extract_active_lambda_param_names_incremental(
                db,
                file,
                data.cursor_offset,
            )
        })
        .unwrap_or_else(|| {
            crate::salsa_queries::extract_active_lambda_param_names_from_source(
                source,
                data.cursor_offset,
            )
        });
    let flow_type_overrides = flow::extract_instanceof_true_branch_overrides(
        &extractor,
        cursor_node,
        &type_ctx,
        &local_variables,
    );
    let statement_labels = convert_statement_labels(&data.statement_labels);
    let functional_target_hint = data
        .functional_target_hint
        .as_ref()
        .map(convert_functional_target_hint);

    let mut ctx = ctx;
    ctx.local_variables = local_variables;

    ctx.with_static_imports(static_imports)
        .with_class_member_position(data.is_class_member_position)
        .with_class_members(members.into_values())
        .with_enclosing_member(enclosing_class_member)
        .with_char_after_cursor(data.char_after_cursor)
        .with_statement_labels(statement_labels)
        .with_active_lambda_param_names(active_lambda_param_names)
        .with_functional_target_hint(functional_target_hint)
        .with_flow_type_overrides(flow_type_overrides)
        .with_extension(type_ctx)
}

fn fetch_class_members_from_workspace(
    db: &dyn Db,
    file: SourceFile,
    workspace: &crate::workspace::Workspace,
    cursor_offset: usize,
) -> HashMap<Arc<str>, CurrentClassMember> {
    crate::salsa_queries::extract_class_members_incremental(db, file, cursor_offset, workspace)
}

fn fetch_static_imports(db: &dyn Db, file: SourceFile) -> Vec<Arc<str>> {
    match file.language_id(db).as_ref() {
        "java" => crate::salsa_queries::java::extract_java_static_imports(db, file),
        _ => Vec::new(),
    }
}

fn convert_cursor_location(data: &CursorLocationData) -> CursorLocation {
    match data {
        CursorLocationData::Expression { prefix } => CursorLocation::Expression {
            prefix: prefix.to_string(),
        },
        CursorLocationData::MemberAccess {
            receiver_expr,
            member_prefix,
            receiver_type_hint,
            arguments,
        } => CursorLocation::MemberAccess {
            receiver_semantic_type: receiver_type_hint
                .as_ref()
                .map(|s| TypeName::new(Arc::clone(s))),
            receiver_type: receiver_type_hint.clone(),
            member_prefix: member_prefix.to_string(),
            receiver_expr: receiver_expr.to_string(),
            arguments: arguments.as_ref().map(|s| s.to_string()),
        },
        CursorLocationData::StaticAccess {
            class_internal_name,
            member_prefix,
        } => CursorLocation::StaticAccess {
            class_internal_name: Arc::clone(class_internal_name),
            member_prefix: member_prefix.to_string(),
        },
        CursorLocationData::Import { prefix } => CursorLocation::Import {
            prefix: prefix.to_string(),
        },
        CursorLocationData::ImportStatic { prefix } => CursorLocation::ImportStatic {
            prefix: prefix.to_string(),
        },
        CursorLocationData::MethodArgument { prefix, .. } => CursorLocation::MethodArgument {
            prefix: prefix.to_string(),
        },
        CursorLocationData::ConstructorCall {
            class_prefix,
            expected_type,
        } => CursorLocation::ConstructorCall {
            class_prefix: class_prefix.to_string(),
            expected_type: expected_type.as_ref().map(|s| s.to_string()),
        },
        CursorLocationData::TypeAnnotation { prefix } => CursorLocation::TypeAnnotation {
            prefix: prefix.to_string(),
        },
        CursorLocationData::VariableName { type_name } => CursorLocation::VariableName {
            type_name: type_name.to_string(),
        },
        CursorLocationData::StringLiteral { prefix } => CursorLocation::StringLiteral {
            prefix: prefix.to_string(),
        },
        CursorLocationData::MethodReference {
            qualifier_expr,
            member_prefix,
            is_constructor,
        } => CursorLocation::MethodReference {
            qualifier_expr: qualifier_expr.to_string(),
            member_prefix: member_prefix.to_string(),
            is_constructor: *is_constructor,
        },
        CursorLocationData::Annotation { prefix } => CursorLocation::Annotation {
            prefix: prefix.to_string(),
            target_element_type: None,
        },
        CursorLocationData::StatementLabel { kind, prefix } => {
            use crate::semantic::context::StatementLabelCompletionKind;

            let completion_kind = match kind {
                crate::salsa_queries::StatementLabelKind::Break => {
                    StatementLabelCompletionKind::Break
                }
                crate::salsa_queries::StatementLabelKind::Continue => {
                    StatementLabelCompletionKind::Continue
                }
            };

            CursorLocation::StatementLabel {
                kind: completion_kind,
                prefix: prefix.to_string(),
            }
        }
        CursorLocationData::Unknown => CursorLocation::Unknown,
    }
}

fn fetch_locals_from_workspace(
    db: &dyn Db,
    file: SourceFile,
    workspace: &crate::workspace::Workspace,
    context: &CompletionContextData,
) -> Vec<LocalVar> {
    crate::salsa_queries::extract_visible_method_locals_incremental(
        db,
        file,
        context.cursor_offset,
        workspace,
    )
}

pub fn convert_local_var(data: &crate::salsa_queries::LocalVarData) -> LocalVar {
    LocalVar {
        name: Arc::clone(&data.name),
        type_internal: TypeName::new(data.type_internal.as_ref()),
        init_expr: data.init_expr.as_ref().map(|s| s.to_string()),
    }
}

fn convert_statement_labels(data: &[StatementLabelData]) -> Vec<StatementLabel> {
    data.iter()
        .map(|label| StatementLabel {
            name: Arc::clone(&label.name),
            target_kind: convert_statement_label_target_kind(label.target_kind),
        })
        .collect()
}

fn convert_statement_label_target_kind(
    kind: StatementLabelTargetKindData,
) -> StatementLabelTargetKind {
    match kind {
        StatementLabelTargetKindData::Block => StatementLabelTargetKind::Block,
        StatementLabelTargetKindData::While => StatementLabelTargetKind::While,
        StatementLabelTargetKindData::DoWhile => StatementLabelTargetKind::DoWhile,
        StatementLabelTargetKindData::For => StatementLabelTargetKind::For,
        StatementLabelTargetKindData::EnhancedFor => StatementLabelTargetKind::EnhancedFor,
        StatementLabelTargetKindData::Switch => StatementLabelTargetKind::Switch,
        StatementLabelTargetKindData::Other => StatementLabelTargetKind::Other,
    }
}

fn convert_functional_target_hint(data: &FunctionalTargetHintData) -> FunctionalTargetHint {
    FunctionalTargetHint {
        expected_type_source: data.expected_type_source.as_deref().map(str::to_owned),
        expected_type_context: data
            .expected_type_context
            .as_ref()
            .map(convert_expected_type_source),
        assignment_lhs_expr: data.assignment_lhs_expr.as_deref().map(str::to_owned),
        method_call: data
            .method_call
            .as_ref()
            .map(convert_functional_method_call_hint),
        expr_shape: data.expr_shape.as_ref().map(convert_functional_expr_shape),
    }
}

fn convert_expected_type_source(data: &ExpectedTypeSourceData) -> ExpectedTypeSource {
    match data {
        ExpectedTypeSourceData::VariableInitializer => ExpectedTypeSource::VariableInitializer,
        ExpectedTypeSourceData::AssignmentRhs => ExpectedTypeSource::AssignmentRhs,
        ExpectedTypeSourceData::ReturnExpr => ExpectedTypeSource::ReturnExpr,
        ExpectedTypeSourceData::MethodArgument { arg_index } => {
            ExpectedTypeSource::MethodArgument {
                arg_index: *arg_index,
            }
        }
    }
}

fn convert_functional_method_call_hint(
    data: &crate::salsa_queries::FunctionalMethodCallHintData,
) -> FunctionalMethodCallHint {
    FunctionalMethodCallHint {
        receiver_expr: data.receiver_expr.as_ref().to_string(),
        method_name: data.method_name.as_ref().to_string(),
        arg_index: data.arg_index,
        arg_texts: data
            .arg_texts
            .iter()
            .map(|arg| arg.as_ref().to_string())
            .collect(),
    }
}

fn convert_functional_expr_shape(data: &FunctionalExprShapeData) -> FunctionalExprShape {
    match data {
        FunctionalExprShapeData::MethodReference {
            qualifier_expr,
            member_name,
            is_constructor,
            qualifier_kind,
        } => FunctionalExprShape::MethodReference {
            qualifier_expr: qualifier_expr.as_ref().to_string(),
            member_name: member_name.as_ref().to_string(),
            is_constructor: *is_constructor,
            qualifier_kind: convert_method_ref_qualifier_kind(*qualifier_kind),
        },
        FunctionalExprShapeData::Lambda {
            param_count,
            expression_body,
        } => FunctionalExprShape::Lambda {
            param_count: *param_count,
            expression_body: expression_body.as_deref().map(str::to_owned),
        },
    }
}

fn convert_method_ref_qualifier_kind(data: MethodRefQualifierKindData) -> MethodRefQualifierKind {
    match data {
        MethodRefQualifierKindData::Type => MethodRefQualifierKind::Type,
        MethodRefQualifierKindData::Expr => MethodRefQualifierKind::Expr,
        MethodRefQualifierKindData::Unknown => MethodRefQualifierKind::Unknown,
    }
}

pub fn convert_field_summary(field: &FieldSummary) -> CurrentClassMember {
    CurrentClassMember::Field(Arc::new(field.clone()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::salsa_db::{Database, FileId, SourceFile};
    use crate::semantic::context::{
        ExpectedTypeSource, FunctionalExprShape, StatementLabelCompletionKind,
        StatementLabelTargetKind,
    };
    use ropey::Rope;
    use tower_lsp::lsp_types::Url;

    #[test]
    fn test_convert_cursor_location_expression() {
        let data = CursorLocationData::Expression {
            prefix: Arc::from("test"),
        };

        let location = convert_cursor_location(&data);

        match location {
            CursorLocation::Expression { prefix } => {
                assert_eq!(prefix, "test");
            }
            _ => panic!("Expected Expression location"),
        }
    }

    #[test]
    fn test_convert_cursor_location_member_access() {
        let data = CursorLocationData::MemberAccess {
            receiver_expr: Arc::from("obj"),
            member_prefix: Arc::from("get"),
            receiver_type_hint: Some(Arc::from("java/lang/Object")),
            arguments: None,
        };

        let location = convert_cursor_location(&data);

        match location {
            CursorLocation::MemberAccess {
                receiver_expr,
                member_prefix,
                receiver_type,
                ..
            } => {
                assert_eq!(receiver_expr, "obj");
                assert_eq!(member_prefix, "get");
                assert_eq!(receiver_type.as_deref(), Some("java/lang/Object"));
            }
            _ => panic!("Expected MemberAccess location"),
        }
    }

    #[test]
    fn test_java_completion_context_conversion_enriches_locals() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let marked_source = indoc::indoc! {r#"
            class Test {
                void demo() {
                    String localValue = "";
                    localV|
                }
            }
        "#};
        let marker = marked_source.find('|').expect("cursor marker");
        let source = marked_source.replacen('|', "", 1);
        let rope = Rope::from_str(&source);
        let line = rope.byte_to_line(marker) as u32;
        let character = (marker - rope.line_to_byte(line as usize)) as u32;
        let file = SourceFile::new(&db, FileId::new(uri), source.clone(), Arc::from("java"));

        let data = crate::salsa_queries::java::extract_java_completion_context(
            &db, file, line, character, None,
        );
        let ctx = SemanticContext::from_salsa_data(data.as_ref().clone(), &db, file, None);

        assert_eq!(ctx.query, "localV");
        assert!(
            ctx.local_variables
                .iter()
                .any(|local| local.name.as_ref() == "localValue"),
            "expected localValue to be present in the converted semantic context"
        );
    }

    #[test]
    fn test_java_completion_context_conversion_preserves_statement_labels() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let marked_source = indoc::indoc! {r#"
            class Test {
                void demo() {
                    outer: while (true) {
                        break out|
                    }
                }
            }
        "#};
        let marker = marked_source.find('|').expect("cursor marker");
        let source = marked_source.replacen('|', "", 1);
        let rope = Rope::from_str(&source);
        let line = rope.byte_to_line(marker) as u32;
        let character = (marker - rope.line_to_byte(line as usize)) as u32;
        let file = SourceFile::new(&db, FileId::new(uri), source.clone(), Arc::from("java"));

        let data = crate::salsa_queries::java::extract_java_completion_context(
            &db, file, line, character, None,
        );
        let ctx = SemanticContext::from_salsa_data(data.as_ref().clone(), &db, file, None);

        match &ctx.location {
            CursorLocation::StatementLabel { kind, prefix } => {
                assert_eq!(*kind, StatementLabelCompletionKind::Break);
                assert_eq!(prefix, "out");
            }
            other => panic!("expected statement-label location, got {other:?}"),
        }
        assert_eq!(
            ctx.statement_labels
                .iter()
                .map(|label| (label.name.as_ref(), label.target_kind))
                .collect::<Vec<_>>(),
            vec![("outer", StatementLabelTargetKind::While)]
        );
    }

    #[test]
    fn test_java_completion_context_conversion_preserves_functional_target_hint() {
        let db = Database::default();
        let uri = Url::parse("file:///test/Test.java").unwrap();
        let marked_source = indoc::indoc! {r#"
            class Test {
                void demo() {
                    java.util.function.Function<String, Integer> fn = s -> s.subs|
                }
            }
        "#};
        let marker = marked_source.find('|').expect("cursor marker");
        let source = marked_source.replacen('|', "", 1);
        let rope = Rope::from_str(&source);
        let line = rope.byte_to_line(marker) as u32;
        let character = (marker - rope.line_to_byte(line as usize)) as u32;
        let file = SourceFile::new(&db, FileId::new(uri), source.clone(), Arc::from("java"));

        let data = crate::salsa_queries::java::extract_java_completion_context(
            &db, file, line, character, None,
        );
        let ctx = SemanticContext::from_salsa_data(data.as_ref().clone(), &db, file, None);
        let hint = ctx
            .functional_target_hint
            .as_ref()
            .expect("expected functional target hint");

        assert_eq!(
            hint.expected_type_context,
            Some(ExpectedTypeSource::VariableInitializer)
        );
        assert!(hint.expected_type_source.is_some());
        match hint.expr_shape.as_ref() {
            Some(FunctionalExprShape::Lambda { param_count, .. }) => {
                assert_eq!(*param_count, 1);
            }
            other => panic!("expected lambda expr shape, got {other:?}"),
        }
    }
}
