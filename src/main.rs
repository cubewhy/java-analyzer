use tower_lsp::{LspService, Server};
use tracing::info;
use tracing_subscriber::{EnvFilter, fmt, prelude::*};

#[tokio::main]
async fn main() {
    tracing_subscriber::registry()
        .with(fmt::layer().with_writer(std::io::stderr).with_ansi(false))
        .with(
            EnvFilter::try_from_env("JAVA_ANALYZER_LOG")
                .unwrap_or_else(|_| EnvFilter::new("debug")),
        )
        .init();

    info!(
        version = env!("CARGO_PKG_VERSION"),
        "java-analyzer starting"
    );

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(java_analyzer::lsp::Backend::new).finish();

    info!("LSP server listening on stdio");
    Server::new(stdin, stdout, socket).serve(service).await;
}
