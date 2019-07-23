use crate::*;

use std::path::{Path, PathBuf};

/// A git tag object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tag {
    /// Commit data
    pub data: KeyValMsg,
    /// Path to repository
    pub repo: PathBuf,
}

impl Tag {
    pub fn to_bytes(&self) -> Vec<u8> {
        obj::Raw::from(self.clone()).to_bytes()
    }

    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        Tag::try_from(obj::Raw::from_bytes(bytes, repo)?)
    }
}
