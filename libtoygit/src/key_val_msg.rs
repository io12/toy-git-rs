use crate::invalid_data_err;

use std::fmt;
use std::io;
use std::str;
use std::str::FromStr;

use indexmap::IndexMap;
use itertools::Itertools;
use regex::Regex;

/// A key value mapping with a message. This is used for commits and
/// tags.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeyValMsg {
    pub map: IndexMap<String, String>,
    pub msg: String,
}

impl KeyValMsg {
    fn error() -> io::Error {
        invalid_data_err("invalid key val msg")
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.to_string().bytes().collect()
    }

    pub fn from_bytes(bytes: &[u8]) -> io::Result<Self> {
        str::from_utf8(bytes).map_err(|_| Self::error())?.parse()
    }
}

impl FromStr for KeyValMsg {
    type Err = io::Error;

    fn from_str(s: &str) -> io::Result<Self> {
        const KV_MSG_DELIM: &'static str = "\n\n";
        lazy_static! {
            // TODO: Document this
            static ref KEY_VAL: Regex = Regex::new(r"(.|\n )+").unwrap();
        }

        let (key_val, msg) = s.split(KV_MSG_DELIM).next_tuple().ok_or_else(Self::error)?;

        let map = KEY_VAL
            .find_iter(key_val)
            .map(|mat| mat.as_str())
            .map(|key_val| key_val.splitn(2, ' ').map(|s| s.to_string()).next_tuple())
            .collect::<Option<IndexMap<String, String>>>()
            .ok_or_else(Self::error)?;

        let msg = msg.trim().to_string();

        Ok(Self { map, msg })
    }
}

impl fmt::Display for KeyValMsg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (key, val) in &self.map {
            write!(f, "{} {}\n", key, val)?;
        }
        write!(f, "\n{}\n", self.msg)
    }
}
