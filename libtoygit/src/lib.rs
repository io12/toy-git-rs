#[macro_use]
extern crate lazy_static;
extern crate libflate;
extern crate sha1;
extern crate tini;

use std::env;
use std::ffi::OsString;
use std::fmt;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::iter::{self, Iterator};
use std::path::{Path, PathBuf};
use std::str::{self, FromStr};

use libflate::zlib;
pub use sha1::Digest as GitHash;
pub use sha1::Sha1 as GitHasher;
use tini::Ini;

/// The type of a git object
#[derive(PartialEq, Eq)]
pub enum GitObjType {
    Blob,
    Commit,
    Tag,
    Tree,
}

/// A git object
#[derive(PartialEq, Eq)]
pub struct GitObj {
    /// Type of the object
    pub typ: GitObjType,
    /// The object's data
    pub data: Vec<u8>,
}

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

impl fmt::Display for GitObjType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            GitObjType::Blob => "blob",
            GitObjType::Commit => "commit",
            GitObjType::Tag => "tag",
            GitObjType::Tree => "tree",
        };

        f.write_str(s)
    }
}

impl FromStr for GitObjType {
    type Err = io::Error;

    /// Parse `GitObjType` from `&str`
    fn from_str(s: &str) -> io::Result<Self> {
        match s {
            "blob" => Ok(GitObjType::Blob),
            "commit" => Ok(GitObjType::Commit),
            "tag" => Ok(GitObjType::Tag),
            "tree" => Ok(GitObjType::Tree),
            _ => Err(invalid_data_err("invalid object type")),
        }
    }
}

impl GitObjType {
    /// Parse from byte slice
    pub fn from_bytes(slice: &[u8]) -> io::Result<Self> {
        let res_s = str::from_utf8(slice);
        let s = res_s.map_err(|_| invalid_data_err("object type is not UTF-8"))?;
        s.parse::<Self>()
    }
}

impl GitObj {
    /// Find the path of a git object from its hash in a given
    /// repository. This always succeeds, since it does not check if
    /// the object exists.
    pub fn find_in_repo(repo: &Path, hash: &GitHash) -> PathBuf {
        let objs = repo.join(GIT_DIR.to_os_string()).join("objects");
        let hash = hash.to_string();
        let (obj_dir, obj_file) = hash.split_at(2);

        objs.join(obj_dir).join(obj_file)
    }

    /// Like `GitObj::find_in_repo()`, but locates the repository with
    /// `find_repo()`
    pub fn find(hash: &GitHash) -> io::Result<PathBuf> {
        Ok(GitObj::find_in_repo(&find_repo()?, hash))
    }

    /// Read the git object with hash `hash` in repo `repo`
    pub fn read_in_repo(repo: &Path, hash: &GitHash) -> io::Result<Self> {
        let path = GitObj::find_in_repo(repo, hash);
        let file = File::open(path)?;

        // Read bytes of decompressed object
        let bytes = zlib::Decoder::new(file)?
            .bytes()
            .collect::<io::Result<Vec<u8>>>()?;

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

        // Parse object type
        let typ = GitObjType::from_bytes(typ)?;

        let data = data.to_vec();

        if data.len() == size {
            Ok(Self { typ, data })
        } else {
            Err(invalid_data_err("git object size corrupted"))
        }
    }

    /// Like `GitObj::read_in_repo()`, but locates the repository with
    /// `find_repo()`
    pub fn read(hash: &GitHash) -> io::Result<Self> {
        Self::read_in_repo(&find_repo()?, hash)
    }

    /// Write the object serialized and zipped to the git directory in
    /// a given repository (usually stored in `repo/.git/objects/xx/xxxxxx...`)
    pub fn write_in_repo(&self, repo: &Path) -> io::Result<()> {
        let hash = self.hash();
        let path = Self::find_in_repo(repo, &hash);
        if let Some(dir) = path.parent() {
            fs::create_dir_all(dir)?;
        }
        let file = File::create(path)?;

        let mut encoder = zlib::Encoder::new(file)?;
        let mut bytes = self.serialize();
        encoder.write_all(&mut bytes)?;
        encoder.finish();
        Ok(())
    }

    /// Like `GitObj::write_in_repo()`, but locates the repository
    /// with `find_repo()`
    pub fn write(&self) -> io::Result<()> {
        self.write_in_repo(&find_repo()?)
    }

    /// Get an object's serialized bytes
    pub fn serialize(&self) -> Vec<u8> {
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

    /// Compute the hash of an object
    pub fn hash(&self) -> GitHash {
        let bytes = self.serialize();
        GitHasher::from(bytes).digest()
    }
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
/// directory. This corresponds to `git hash-object`.
///
/// * `path` - path to the file
/// * `typ` - type of the object
/// * `do_write` - whether to create the object
pub fn hash_object(path: &Path, typ: GitObjType, do_write: bool) -> io::Result<GitHash> {
    let data = fs::read(path)?;
    let obj = GitObj { typ, data };
    if do_write {
        obj.write()?;
    }
    Ok(obj.hash())
}
