mod gref;
mod key_val_msg;
mod log;
pub mod obj;

pub use crate::log::Log;
pub use gref::Ref;
pub use key_val_msg::KeyValMsg;
pub use obj::Obj;

#[macro_use]
extern crate lazy_static;
extern crate indexmap;
extern crate itertools;
extern crate libflate;
#[macro_use]
extern crate log as log_crate;
extern crate regex;
extern crate sha1;
extern crate tini;

use std::convert::TryFrom;
use std::env;
use std::ffi::OsString;
use std::fmt;
use std::fs;
use std::io::{self, Read, Write};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::str::{self, FromStr};

use itertools::Itertools;
pub use sha1::Digest as Hash;
pub use sha1::Sha1 as Hasher;
use tini::Ini;

/// Length of byte representation of `Hash`
pub const HASH_LEN: usize = 20;

const DEFAULT_DESCRIPTION: &str =
    "Unnamed repository; edit this file 'description' to name the repository.\n";
const DEFAULT_HEAD: &str = "ref: refs/heads/master\n";

lazy_static! {
    static ref GIT_DIR: OsString = env::var_os("GIT_DIR").unwrap_or_else(|| OsString::from(".git"));
    static ref DEFAULT_CONFIG: Ini = Ini::new()
        .section("core")
        .item("repositoryformatversion", "0")
        .item("filemode", "true")
        .item("bare", "false")
        .item("logallrefupdates", "true");
}

/// Use a predicate to split a slice once into a tuple of the left and
/// right sub-slices.
// TODO: Refactor into a separate crate?
fn split_once<T, F>(slice: &[T], pred: F) -> Option<(&[T], &[T])>
where
    F: FnMut(&T) -> bool,
{
    let mut iter = slice.splitn(2, pred);
    Some((iter.next()?, iter.next()?))
}

fn invalid_data_err(msg: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, msg)
}

fn obj_mismatch_err() -> io::Error {
    invalid_data_err("object type mismatch")
}

/// Find the repository root by traversing the file tree upwards. This
/// directory should contain `.git/` or `$GIT_DIR`.
pub fn find_repo() -> io::Result<PathBuf> {
    let cwd = env::current_dir()?;
    let git_dir = GIT_DIR.to_os_string();

    let mut path = cwd.as_path();

    while !path.join(&git_dir).is_dir() {
        path = path
            .parent()
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "repository not found"))?;
    }

    Ok(path.to_path_buf())
}

/// Write an `Ini` object to a file with a trailing newline
fn write_ini(ini: &Ini, path: &Path) -> io::Result<()> {
    let mut s = ini.to_buffer();
    s.push('\n');
    fs::write(path, s)
}

/// Create an empty Git repository or reinitialize an existing one.
/// This corresponds to `git init`.
///
/// * `dir` - path to repository
pub fn init(dir: &Path) -> io::Result<()> {
    let git_dir = dir.join(GIT_DIR.to_os_string());
    let refs_dir = git_dir.join("refs");

    fs::create_dir(&git_dir)?;
    fs::create_dir(&refs_dir)?;

    fs::create_dir(git_dir.join("branches"))?;
    fs::create_dir(git_dir.join("objects"))?;

    fs::create_dir(refs_dir.join("heads"))?;
    fs::create_dir(refs_dir.join("tags"))?;

    write_ini(&DEFAULT_CONFIG, &git_dir.join("config"))?;
    fs::write(git_dir.join("description"), DEFAULT_DESCRIPTION)?;
    fs::write(git_dir.join("HEAD"), DEFAULT_HEAD)?;

    Ok(())
}

/// Compute the hash that an object containing the data in a given
/// file would have, optionally creating the object in the git
/// directory. The repository is located with `find_repo()`. This
/// function corresponds to `git hash-object`.
///
/// * `path` - path to the file
/// * `typ` - type of the object
/// * `do_write` - whether to create the object
pub fn hash_object(path: &Path, typ: obj::Kind, do_write: bool) -> io::Result<Hash> {
    let obj = obj::Raw {
        typ,
        data: fs::read(path)?,
        repo: find_repo()?,
    };
    let obj = Obj::try_from(obj)?;
    if do_write {
        obj.write()?;
    }
    Ok(obj.hash())
}

/// Find the path of a git object from its hash in a given
/// repository. This always succeeds, since it does not check if
/// the object exists.
pub fn find_object_in_repo(repo: &Path, hash: &Hash) -> PathBuf {
    let objs = repo.join(GIT_DIR.to_os_string()).join("objects");
    let hash = hash.to_string();
    let (obj_dir, obj_file) = hash.split_at(2);

    objs.join(obj_dir).join(obj_file)
}

/// Like `find_object_in_repo()`, but locates the repository with
/// `find_repo()`
pub fn find_object(hash: &Hash) -> io::Result<PathBuf> {
    Ok(find_object_in_repo(&find_repo()?, hash))
}
