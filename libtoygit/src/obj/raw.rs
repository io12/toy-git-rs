use crate::*;
use obj::Obj;

use std::iter;

/// A raw, untyped git object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Raw {
    /// Type of object
    pub typ: obj::Kind,
    /// Raw bytes of object body
    pub data: Vec<u8>,
    /// Path to repository
    pub repo: PathBuf,
}

impl From<Obj> for obj::Raw {
    fn from(obj: Obj) -> Self {
        match obj {
            Obj::Blob(o) => Self::from(o),
            Obj::Commit(o) => Self::from(o),
            Obj::Tag(o) => Self::from(o),
            Obj::Tree(o) => Self::from(o),
        }
    }
}

impl From<obj::Blob> for obj::Raw {
    fn from(obj: obj::Blob) -> Self {
        Self {
            typ: obj::Kind::Blob,
            data: obj.data,
            repo: obj.repo,
        }
    }
}

impl TryFrom<obj::Raw> for obj::Blob {
    type Error = io::Error;

    fn try_from(obj: obj::Raw) -> io::Result<Self> {
        if obj.typ == obj::Kind::Blob {
            Ok(obj::Blob {
                data: obj.data,
                repo: obj.repo,
            })
        } else {
            Err(obj_mismatch_err())
        }
    }
}

impl From<obj::Commit> for obj::Raw {
    fn from(obj: obj::Commit) -> Self {
        Self {
            typ: obj::Kind::Commit,
            data: obj.data.to_bytes(),
            repo: obj.repo,
        }
    }
}

impl TryFrom<obj::Raw> for obj::Commit {
    type Error = io::Error;

    fn try_from(obj: obj::Raw) -> io::Result<Self> {
        if obj.typ == obj::Kind::Commit {
            Ok(obj::Commit {
                data: KeyValMsg::from_bytes(&obj.data)?,
                repo: obj.repo,
            })
        } else {
            Err(obj_mismatch_err())
        }
    }
}

impl TryFrom<Obj> for obj::Commit {
    type Error = io::Error;

    fn try_from(obj: Obj) -> io::Result<Self> {
        if let Obj::Commit(commit) = obj {
            Ok(commit)
        } else {
            Err(obj_mismatch_err())
        }
    }
}

impl From<obj::Tag> for obj::Raw {
    fn from(obj: obj::Tag) -> Self {
        Self {
            typ: obj::Kind::Commit,
            data: obj.data.to_bytes(),
            repo: obj.repo,
        }
    }
}

impl TryFrom<obj::Raw> for obj::Tag {
    type Error = io::Error;

    fn try_from(obj: obj::Raw) -> io::Result<Self> {
        if obj.typ == obj::Kind::Commit {
            Ok(obj::Tag {
                data: KeyValMsg::from_bytes(&obj.data)?,
                repo: obj.repo,
            })
        } else {
            Err(obj_mismatch_err())
        }
    }
}

impl From<obj::Tree> for obj::Raw {
    fn from(obj: obj::Tree) -> Self {
        Self {
            typ: obj::Kind::Tree,
            data: obj
                .nodes
                .iter()
                .map(obj::tree::Node::to_bytes)
                .flatten()
                .collect::<Vec<u8>>(),
            repo: obj.repo,
        }
    }
}

impl TryFrom<obj::Raw> for obj::Tree {
    type Error = io::Error;

    fn try_from(obj: obj::Raw) -> io::Result<Self> {
        if obj.typ == obj::Kind::Tree {
            Ok(obj::Tree {
                nodes: obj::tree::Node::from_bytes_nodes(&obj.data)?,
                repo: obj.repo,
            })
        } else {
            Err(obj_mismatch_err())
        }
    }
}

impl obj::Raw {
    pub fn to_bytes(&self) -> Vec<u8> {
        let typ = self.typ.to_string();
        let typ = typ.bytes();

        let data = self.data.iter().cloned();

        let size = self.data.len().to_string();
        let size = size.bytes();

        typ.chain(iter::once(b' '))
            .chain(size)
            .chain(iter::once(b'\x00'))
            .chain(data)
            .collect()
    }

    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        // Split object at null byte
        let (header, data) = split_once(&bytes, |&b| b == b'\x00')
            .ok_or_else(|| invalid_data_err("no null byte in object"))?;

        // Split header at space
        let (typ, size) = split_once(&header, |&b| b == b' ')
            .ok_or_else(|| invalid_data_err("no space in git object header"))?;

        // Convert size from `&[u8]` to `&str`
        let size = str::from_utf8(size)
            .map_err(|_| invalid_data_err("git object size is not valid UTF-8"))?;

        // Convert size from `&str` to `usize`
        let size = size
            .parse::<usize>()
            .map_err(|_| invalid_data_err("git object size is not an integer"))?;

        if data.len() == size {
            Ok(Self {
                // Parse object type
                typ: obj::Kind::try_from(typ)?,
                data: data.to_vec(),
                repo: repo.to_path_buf(),
            })
        } else {
            Err(invalid_data_err("git object size corrupted"))
        }
    }
}

impl TryFrom<obj::Raw> for Obj {
    type Error = io::Error;

    fn try_from(raw: obj::Raw) -> io::Result<Self> {
        match raw.typ {
            obj::Kind::Blob => obj::Blob::try_from(raw).map(|o| Obj::Blob(o)),
            obj::Kind::Commit => obj::Commit::try_from(raw).map(|o| Obj::Commit(o)),
            obj::Kind::Tag => obj::Tag::try_from(raw).map(|o| Obj::Tag(o)),
            obj::Kind::Tree => obj::Tree::try_from(raw).map(|o| Obj::Tree(o)),
        }
    }
}
