#[macro_use]
extern crate lazy_static;
extern crate compress;
extern crate sha1;
extern crate tini;

use std::env;
use std::ffi::OsString;
use std::fs::{self, File};
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::str;

use compress::zlib;
pub use sha1::Digest as ObjHash;
use tini::Ini;

/// The type of a git object
pub enum ObjType {
    Blob,
    Commit,
    Tag,
    Tree,
}

/// A git object
pub struct Obj {
    /// Type of the object
    pub typ: ObjType,
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

impl ObjType {
    /// Parse from byte slice
    fn from_bytes(slice: &[u8]) -> io::Result<Self> {
        match str::from_utf8(slice) {
            Ok("blob") => Ok(ObjType::Blob),
            Ok("commit") => Ok(ObjType::Commit),
            Ok("tag") => Ok(ObjType::Tag),
            Ok("tree") => Ok(ObjType::Tree),
            _ => Err(invalid_data_err("invalid object type")),
        }
    }
}

impl Obj {
    /// Find the path of a git object from its hash in a given
    /// repository. This always succeeds, since it does not check if
    /// the object exists.
    pub fn find_in_repo(repo: &Path, hash: &ObjHash) -> PathBuf {
        let objs = repo.join(GIT_DIR.to_os_string()).join("objects");
        let hash = hash.to_string();
        let (obj_dir, obj_file) = hash.split_at(2);

        objs.join(obj_dir).join(obj_file)
    }

    /// Like `Obj::find_in_repo()`, but locates the repository with
    /// `find_repo()`
    pub fn find(hash: &ObjHash) -> io::Result<PathBuf> {
        Ok(Obj::find_in_repo(&find_repo()?, hash))
    }

    /// Read the git object with hash `hash` in repo `repo`
    pub fn read_in_repo(repo: &Path, hash: &ObjHash) -> io::Result<Self> {
        let path = Obj::find_in_repo(repo, hash);
        let file = File::open(path)?;

        // Read bytes of decompressed object
        let bytes = zlib::Decoder::new(file)
            .bytes()
            .collect::<Result<Vec<u8>, _>>()?;

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
        let typ = ObjType::from_bytes(typ)?;

        let data = data.to_vec();

        if data.len() == size {
            Ok(Self { typ, data })
        } else {
            Err(invalid_data_err("git object size corrupted"))
        }
    }

    /// Like `Obj::read_in_repo()`, but locates the repository with
    /// `find_repo()`
    pub fn read(hash: &ObjHash) -> io::Result<Self> {
        Obj::read_in_repo(&find_repo()?, hash)
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
