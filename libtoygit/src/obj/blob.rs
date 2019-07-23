use crate::*;

use std::io;
use std::path::{Path, PathBuf};

/// A git blob object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Blob {
    /// Data in blob
    pub data: Vec<u8>,
    /// Path to repository
    pub repo: PathBuf,
}

impl Blob {
    pub fn to_bytes(&self) -> Vec<u8> {
        obj::Raw::from(self.clone()).to_bytes()
    }

    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        let raw = obj::Raw::from_bytes(bytes, repo)?;
        if raw.typ == obj::Kind::Blob {
            Ok(Blob {
                data: raw.data,
                repo: raw.repo,
            })
        } else {
            Err(obj_mismatch_err())
        }
    }
}
