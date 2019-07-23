use crate::*;

use std::fmt;
use std::io;

/// The type of a git object
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Blob,
    Commit,
    Tag,
    Tree,
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Kind::Blob => "blob",
            Kind::Commit => "commit",
            Kind::Tag => "tag",
            Kind::Tree => "tree",
        };

        f.write_str(s)
    }
}

impl FromStr for Kind {
    type Err = io::Error;

    fn from_str(s: &str) -> io::Result<Self> {
        match s {
            "blob" => Ok(Kind::Blob),
            "commit" => Ok(Kind::Commit),
            "tag" => Ok(Kind::Tag),
            "tree" => Ok(Kind::Tree),
            _ => Err(invalid_data_err("invalid object type")),
        }
    }
}

impl TryFrom<&[u8]> for Kind {
    type Error = io::Error;

    /// Parse from byte slice
    // TODO: make this not use `TryFrom`
    fn try_from(bytes: &[u8]) -> io::Result<Self> {
        let res_s = str::from_utf8(bytes);
        let s = res_s.map_err(|_| invalid_data_err("object type is not UTF-8"))?;
        s.parse::<Self>()
    }
}
