use anyhow::{Context, Result};
use rkyv::{Archive, Deserialize as RkyvDeserialize, Serialize as RkyvSerialize};

use crate::index::{ArchiveClassStub, IndexedArchiveData, IndexedJavaModule};

use super::{ArtifactFingerprint, ArtifactId, ArtifactKind, ArtifactMetadata};

#[derive(Archive, RkyvSerialize, RkyvDeserialize, Debug, Clone, PartialEq, Eq)]
#[archive(check_bytes)]
pub(crate) struct ArtifactMetaV1 {
    pub id: u64,
    pub kind: StoredArtifactKind,
    pub source_path: String,
    pub content_hash: u64,
    pub byte_len: u64,
    pub stored_at_unix_secs: u64,
}

#[derive(Archive, RkyvSerialize, RkyvDeserialize, Debug, Clone, PartialEq)]
#[archive(check_bytes)]
pub(crate) struct ArtifactPayloadV1 {
    pub classes: Vec<ArchiveClassStub>,
    pub modules: Vec<IndexedJavaModule>,
}

#[derive(Archive, RkyvSerialize, RkyvDeserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[archive(check_bytes)]
pub(crate) enum StoredArtifactKind {
    Jar,
    JdkModulesImage,
    SourceZip,
    Unknown,
}

impl From<ArtifactKind> for StoredArtifactKind {
    fn from(value: ArtifactKind) -> Self {
        match value {
            ArtifactKind::Jar => Self::Jar,
            ArtifactKind::JdkModulesImage => Self::JdkModulesImage,
            ArtifactKind::SourceZip => Self::SourceZip,
            ArtifactKind::Unknown => Self::Unknown,
        }
    }
}

impl From<StoredArtifactKind> for ArtifactKind {
    fn from(value: StoredArtifactKind) -> Self {
        match value {
            StoredArtifactKind::Jar => Self::Jar,
            StoredArtifactKind::JdkModulesImage => Self::JdkModulesImage,
            StoredArtifactKind::SourceZip => Self::SourceZip,
            StoredArtifactKind::Unknown => Self::Unknown,
        }
    }
}

impl ArtifactMetaV1 {
    pub(crate) fn from_public(meta: &ArtifactMetadata) -> Self {
        Self {
            id: meta.id.0,
            kind: meta.kind.into(),
            source_path: meta.source_path.clone(),
            content_hash: meta.content_hash,
            byte_len: meta.byte_len,
            stored_at_unix_secs: meta.stored_at_unix_secs,
        }
    }

    pub(crate) fn into_public(self) -> ArtifactMetadata {
        ArtifactMetadata {
            id: ArtifactId(self.id),
            kind: self.kind.into(),
            source_path: self.source_path,
            content_hash: self.content_hash,
            byte_len: self.byte_len,
            stored_at_unix_secs: self.stored_at_unix_secs,
        }
    }
}

impl From<&IndexedArchiveData> for ArtifactPayloadV1 {
    fn from(value: &IndexedArchiveData) -> Self {
        Self {
            classes: value
                .classes
                .iter()
                .cloned()
                .map(ArchiveClassStub::from_class_metadata)
                .collect(),
            modules: value.modules.clone(),
        }
    }
}

impl From<ArtifactPayloadV1> for IndexedArchiveData {
    fn from(value: ArtifactPayloadV1) -> Self {
        Self {
            classes: value
                .classes
                .into_iter()
                .map(|stub| stub.materialize())
                .collect(),
            modules: value.modules,
        }
    }
}

pub(crate) fn serialize_meta(meta: &ArtifactMetaV1) -> Result<Vec<u8>> {
    rkyv::to_bytes::<_, 4096>(meta)
        .map(|bytes| bytes.to_vec())
        .context("serialize artifact metadata with rkyv")
}

pub(crate) fn deserialize_meta(bytes: &[u8]) -> Result<ArtifactMetaV1> {
    rkyv::from_bytes::<ArtifactMetaV1>(bytes)
        .map_err(|err| anyhow::anyhow!("deserialize artifact metadata: {err:?}"))
}

pub(crate) fn serialize_payload(payload: &ArtifactPayloadV1) -> Result<Vec<u8>> {
    rkyv::to_bytes::<_, 4096>(payload)
        .map(|bytes| bytes.to_vec())
        .context("serialize artifact payload with rkyv")
}

pub(crate) fn deserialize_payload(bytes: &[u8]) -> Result<ArtifactPayloadV1> {
    rkyv::from_bytes::<ArtifactPayloadV1>(bytes)
        .map_err(|err| anyhow::anyhow!("deserialize artifact payload: {err:?}"))
}

pub(crate) fn fingerprint_payload(data: &IndexedArchiveData) -> Result<ArtifactFingerprint> {
    use std::hash::Hasher;

    let payload = ArtifactPayloadV1::from(data);
    let bytes = serialize_payload(&payload)?;
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    hasher.write(bytes.as_ref());

    Ok(ArtifactFingerprint {
        content_hash: hasher.finish(),
        byte_len: bytes.len() as u64,
    })
}
