#[macro_use]
extern crate lazy_static;
extern crate indexmap;
extern crate itertools;
extern crate libflate;
#[macro_use]
extern crate log;
extern crate regex;
extern crate sha1;
extern crate tini;

use std::convert::TryFrom;
use std::env;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::iter::{self, Iterator};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::str::{self, FromStr};

use indexmap::map::IndexMap;
use itertools::Itertools;
use libflate::zlib;
use regex::Regex;
pub use sha1::Digest as GitHash;
pub use sha1::Sha1 as GitHasher;
use tini::Ini;

/// Length of byte representation of `GitHash`
pub const HASH_LEN: usize = 20;

/// An iterator over nodes of a git tree.
#[derive(Debug)]
pub struct WalkTree {
    item_stack: Vec<WalkTreeItem>,
}

/// Type returned by iterating over `WalkTree`. It contains a
/// `GitTreeNode` and the path to its parent.
#[derive(Debug)]
pub struct WalkTreeItem {
    /// Tree node
    pub node: GitTreeNode,
    /// Path to node's parent relative to the repository. Note that
    /// `GitTreeNode` only stores the filename, which is why this is
    /// needed.
    pub parent_path: PathBuf,
    /// Path to repository
    pub repo: PathBuf,
}

/// An iterator (log) of commits. This corresponds to `git log`.
#[derive(Debug)]
pub struct GitLog {
    /// Hash of next commit
    next_commit_hash: Option<io::Result<GitHash>>,
    /// Path to repository
    repo: PathBuf,
}

/// A git reference. This is the parsed version of a text file which
/// can either be a SHA1 hash or a path to another reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GitRef {
    Hash {
        /// Repository containing the reference
        repo: PathBuf,
        /// An object hash
        hash: GitHash,
    },
    Ref {
        /// Repository containing the reference
        repo: PathBuf,
        /// Path to next reference
        path: PathBuf,
    },
}

/// The type of a git object
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GitObjType {
    Blob,
    Commit,
    Tag,
    Tree,
}

/// A git blob object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GitBlob {
    /// Data in blob
    pub data: Vec<u8>,
    /// Path to repository
    pub repo: PathBuf,
}

/// A git commit object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GitCommit {
    /// Commit data
    pub data: GitKeyValMsg,
    /// Path to repository
    pub repo: PathBuf,
}

/// A git tag object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GitTag {
    /// Commit data
    pub data: GitKeyValMsg,
    /// Path to repository
    pub repo: PathBuf,
}

/// Node of a git tree object. This represents a file or directory in
/// a tree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GitTreeNode {
    /// Permission bits
    pub mode: fs::Permissions,
    /// Name of file or directory
    pub filename: OsString,
    /// Hash of a git object, which can be either a tree (for
    /// directories) or a blob (for files)
    pub hash: GitHash,
}

/// A git tree object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GitTree {
    /// Nodes of the tree
    pub nodes: Vec<GitTreeNode>,
    /// Path to repository
    pub repo: PathBuf,
}

/// A raw, untyped git object
#[derive(Debug, Clone, PartialEq, Eq)]
struct GitObjRaw {
    /// Type of object
    pub typ: GitObjType,
    /// Raw bytes of object body
    pub data: Vec<u8>,
    /// Path to repository
    pub repo: PathBuf,
}

/// A git object
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GitObj {
    Blob(GitBlob),
    Commit(GitCommit),
    Tag(GitTag),
    Tree(GitTree),
}

/// A key value mapping with a message. This is used for commits and
/// tags.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GitKeyValMsg {
    pub map: IndexMap<String, String>,
    pub msg: String,
}

/// A git object
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GitObj {
    Blob(GitBlob),
    Commit(GitCommit),
    Tag(GitTag),
    Tree(GitTree),
}

/// A key value mapping with a message. This is used for commits and
/// tags.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GitKeyValMsg {
    pub map: IndexMap<String, String>,
    pub msg: String,
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

impl Iterator for WalkTree {
    type Item = io::Result<WalkTreeItem>;

    fn next(&mut self) -> Option<Self::Item> {
        let opt_item = self.item_stack.pop();
        match opt_item {
            Some(item) => match item.children() {
                Ok(mut children) => {
                    self.item_stack.append(&mut children);
                    Some(Ok(item))
                }
                Err(err) => Some(Err(err)),
            },
            None => None,
        }
    }
}

impl WalkTreeItem {
    /// Get the path to the file or directory described by the node.
    /// This path is relative to the repository root.
    pub fn path(&self) -> PathBuf {
        let filename = self.node.filename.to_os_string();
        self.parent_path.join(filename)
    }

    /// Get a list of the node's children. If this node is not a
    /// directory, then the returned vector is empty.
    pub fn children(&self) -> io::Result<Vec<Self>> {
        let obj = GitObj::read_in_repo(&self.repo, &self.node.hash)?;

        match obj {
            // Node is file
            GitObj::Blob(_) => Ok(Vec::new()),
            // Node is directory
            GitObj::Tree(tree) => Ok(tree
                .nodes
                .iter()
                .map(|node| Self {
                    node: node.clone(),
                    parent_path: self.path(),
                    repo: self.repo.clone(),
                })
                .collect()),
            _ => Err(obj_mismatch_err()),
        }
    }
}

impl GitLog {
    /// Get an iterator over a repository's commits from newest to
    /// oldest, beginning from `hash`.
    pub fn from_hash_repo(hash: &GitHash, repo: &Path) -> Self {
        Self {
            next_commit_hash: Some(Ok(*hash)),
            repo: repo.to_path_buf(),
        }
    }

    /// Like `GitLog::from_hash_repo()`, but locate the repository
    /// with `find_repo()`
    pub fn from_hash(hash: &GitHash) -> io::Result<Self> {
        Ok(Self::from_hash_repo(hash, &find_repo()?))
    }

    /// Like `GitLog::from_hash_repo()`, but with `HEAD` as the commit
    /// hash.
    pub fn from_repo(repo: &Path) -> io::Result<Self> {
        let commit = GitCommit::head_in_repo(repo)?;
        let obj = GitObj::Commit(commit);
        let hash = obj.hash();

        Ok(Self::from_hash_repo(&hash, repo))
    }

    /// Like `GitLog::from_hash_repo()`, but with `HEAD` as the commit
    /// hash and locate the repository with `find_repo()`
    pub fn new() -> io::Result<Self> {
        let repo = find_repo()?;

        Self::from_repo(&repo)
    }
}

impl Iterator for GitLog {
    type Item = io::Result<GitCommit>;

    // TODO: Refactor
    fn next(&mut self) -> Option<Self::Item> {
        match &self.next_commit_hash {
            Some(Ok(hash)) => {
                let obj = GitObj::read_in_repo(&self.repo, &hash);
                let commit = obj.and_then(|o| GitCommit::try_from(o));
                self.next_commit_hash = match commit {
                    Ok(ref commit) => match commit.parent_in_repo(&self.repo) {
                        Some(Ok(commit)) => Some(Ok(GitObj::Commit(commit).hash())),
                        Some(Err(err)) => Some(Err(err)),
                        None => None,
                    },
                    Err(_) => Some(Err(obj_mismatch_err())), // TODO: Change error
                };
                Some(commit)
            }
            Some(Err(_)) => {
                self.next_commit_hash = None;
                Some(Err(obj_mismatch_err())) // TODO: Change error
            }
            None => None,
        }
    }
}

impl GitRef {
    fn error() -> io::Error {
        invalid_data_err("failed to parse ref")
    }

    /// Recursively follow reference, yielding target object
    pub fn telescope(&self) -> io::Result<GitObj> {
        match self {
            GitRef::Hash { hash, repo } => GitObj::read_in_repo(&repo, &hash),
            GitRef::Ref { path, repo } => GitRef::read_in_repo(&repo, &path)?.telescope(),
        }
    }

    /// Read a git reference from a file in a repository
    pub fn read_in_repo(repo: &Path, path: &Path) -> io::Result<Self> {
        let git_dir = GIT_DIR.to_os_string();
        let path = repo.join(git_dir).join(path);

        debug!("reading file: {:?}", path);
        Self::from_bytes(&fs::read(path)?, repo)
    }

    /// Like `GitRef::read_in_repo()`, but locate the repo with
    /// `find_repo()`
    pub fn read(path: &Path) -> io::Result<Self> {
        let repo = find_repo()?;
        Self::read_in_repo(&repo, path)
    }

    /// Like `GitRef::read_head_in_repo()`, but finds repository with
    /// `find_repo()`
    pub fn read_head() -> io::Result<Self> {
        let repo = find_repo()?;
        Self::read_head_in_repo(&repo)
    }

    /// Read the git reference to HEAD in a given repository
    pub fn read_head_in_repo(repo: &Path) -> io::Result<Self> {
        let path = repo.join(GIT_DIR.clone()).join("HEAD");
        Self::read(&path)
    }
}

impl GitRef {
    pub fn from_str(s: &str, repo: &Path) -> io::Result<Self> {
        let repo = repo.to_path_buf();

        match s.split(' ').next_tuple() {
            Some(("ref:", path)) => Ok(GitRef::Ref {
                path: PathBuf::from(path.trim()),
                repo,
            }),
            _ => {
                let hash = s.trim();
                let hash = GitHash::from_str(hash);
                let hash = hash.map_err(|_| GitRef::error())?;
                let hash = GitRef::Hash { hash, repo };
                Ok(hash)
            }
        }
    }
}

impl fmt::Display for GitRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GitRef::Hash { hash, .. } => write!(f, "{}", hash),
            GitRef::Ref { path, .. } => write!(f, "{}", path.display()),
        }
    }
}

// TODO: remove boilerplate with derive
impl GitRef {
    pub fn to_bytes(&self) -> Vec<u8> {
        self.to_string().bytes().collect()
    }

    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        let res = str::from_utf8(bytes);
        let res = res.map_err(|_| GitRef::error());
        let s = res?;
        Self::from_str(s, repo)
    }
}

impl GitObj {
    /// Read the git object with hash `hash` in repo `repo`
    pub fn read_in_repo(repo: &Path, hash: &GitHash) -> io::Result<Self> {
        let path = find_object_in_repo(repo, hash);
        debug!("opening file {:?}", path);
        let file = File::open(path)?;

        // Read bytes of decompressed object
        let bytes = zlib::Decoder::new(file)?
            .bytes()
            .collect::<io::Result<Vec<u8>>>()?;

        Self::from_bytes(&bytes, repo)
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
        let path = find_object_in_repo(repo, &hash);
        if let Some(dir) = path.parent() {
            debug!("making dir {:?}", dir);
            fs::create_dir_all(dir)?;
        }
        debug!("creating file {:?}", path);
        let file = File::create(path)?;

        let mut encoder = zlib::Encoder::new(file)?;
        let mut bytes = self.to_bytes();
        encoder.write_all(&mut bytes)?;
        encoder.finish();
        Ok(())
    }

    /// Like `GitObj::write_in_repo()`, but locates the repository
    /// with `find_repo()`
    pub fn write(&self) -> io::Result<()> {
        self.write_in_repo(&find_repo()?)
    }

    /// Compute the hash of an object
    pub fn hash(&self) -> GitHash {
        let bytes = self.to_bytes();
        GitHasher::from(bytes).digest()
    }

    /// Convert to `GitObjRaw`
    fn raw(&self) -> GitObjRaw {
        GitObjRaw::from(self.clone())
    }

    /// Get the type of an object
    pub fn typ(&self) -> GitObjType {
        self.raw().typ
    }

    /// Get the raw data of an object
    pub fn data(&self) -> Vec<u8> {
        self.raw().data
    }
}

impl GitKeyValMsg {
    fn error() -> io::Error {
        invalid_data_err("invalid key val msg")
    }
}

impl FromStr for GitKeyValMsg {
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

impl GitKeyValMsg {
    pub fn to_bytes(&self) -> Vec<u8> {
        self.to_string().bytes().collect()
    }

    pub fn from_bytes(bytes: &[u8]) -> io::Result<Self> {
        str::from_utf8(bytes).map_err(|_| Self::error())?.parse()
    }
}

impl fmt::Display for GitKeyValMsg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (key, val) in &self.map {
            write!(f, "{} {}\n", key, val)?;
        }
        write!(f, "\n{}\n", self.msg)
    }
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

impl TryFrom<&[u8]> for GitObjType {
    type Error = io::Error;

    /// Parse from byte slice
    fn try_from(bytes: &[u8]) -> io::Result<Self> {
        let res_s = str::from_utf8(bytes);
        let s = res_s.map_err(|_| invalid_data_err("object type is not UTF-8"))?;
        s.parse::<Self>()
    }
}

impl GitTreeNode {
    fn error() -> io::Error {
        invalid_data_err("invalid tree node")
    }

    /// Given a byte slice containing an encoded `GitTreeNode`, find
    /// its size in bytes.
    fn byte_bound(bytes: &[u8]) -> Option<usize> {
        bytes
            .iter()
            .position(|&b| b == b'\x00')
            .map(|n| n + HASH_LEN)
            .filter(|&n| n < bytes.len())
    }

    /// Deserialize a `GitTreeNode` from bytes, returning the
    /// `GitTreeNode` and the rest of the bytes
    fn from_bytes_rest<'a>(bytes: &'a [u8]) -> io::Result<(GitTreeNode, &'a [u8])> {
        let opt_n = GitTreeNode::byte_bound(bytes);
        match opt_n {
            Some(n) => {
                let (node_bytes, rest) = bytes.split_at(n);
                let node = GitTreeNode::from_bytes(node_bytes)?;
                Ok((node, rest))
            }
            None => Err(GitTreeNode::error()),
        }
    }

    /// Deserialize `GitTreeNode`s from a byte slice
    fn from_bytes_nodes(mut bytes: &[u8]) -> io::Result<Vec<Self>> {
        let mut ret = Vec::new();
        while bytes.len() != 0 {
            let (node, rest) = Self::from_bytes_rest(bytes)?;
            bytes = rest;
            ret.push(node);
        }
        Ok(ret)
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mode = self.mode.mode();
        let mode = mode.to_string();
        let mode = mode.bytes();

        let path = self.filename.as_bytes().iter().cloned();

        let hash = self.hash.bytes();
        let hash = hash.iter().cloned();

        mode.chain(iter::once(b' '))
            .chain(path)
            .chain(iter::once(b'\x00'))
            .chain(hash)
            .collect()
    }

    pub fn from_bytes(bytes: &[u8]) -> io::Result<Self> {
        let toks = bytes.split(|&b| b == b' ' || b == b'\x00').next_tuple();
        match toks {
            Some((mode, filename, hash)) => {
                // TODO: Refactor
                let mode = str::from_utf8(mode);
                let mode = mode.map_err(|_| Self::error())?;
                let mode = mode.parse::<u32>();
                let mode = mode.map_err(|_| Self::error())?;
                let mode = fs::Permissions::from_mode(mode);

                let filename = OsStr::from_bytes(filename).to_os_string();

                let hash = str::from_utf8(hash);
                let hash = hash.map_err(|_| Self::error())?;
                let hash = GitHash::from_str(hash);
                let hash = hash.map_err(|_| Self::error())?;

                Ok(Self {
                    mode,
                    filename,
                    hash,
                })
            }
            None => Err(Self::error()),
        }
    }
}

impl GitObj {
    pub fn to_bytes(&self) -> Vec<u8> {
        GitObjRaw::from(self.clone()).to_bytes()
    }

    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        let raw = GitObjRaw::from_bytes(bytes, repo)?;
        let obj = GitObj::try_from(raw)?;
        Ok(obj)
    }
}

impl From<GitObj> for GitObjRaw {
    fn from(obj: GitObj) -> Self {
        match obj {
            GitObj::Blob(o) => Self::from(o),
            GitObj::Commit(o) => Self::from(o),
            GitObj::Tag(o) => Self::from(o),
            GitObj::Tree(o) => Self::from(o),
        }
    }
}

impl From<GitBlob> for GitObjRaw {
    fn from(obj: GitBlob) -> Self {
        Self {
            typ: GitObjType::Blob,
            data: obj.data,
            repo: obj.repo,
        }
    }
}

impl TryFrom<GitObjRaw> for GitBlob {
    type Error = io::Error;

    fn try_from(obj: GitObjRaw) -> io::Result<Self> {
        if obj.typ == GitObjType::Blob {
            Ok(GitBlob {
                data: obj.data,
                repo: obj.repo,
            })
        } else {
            Err(obj_mismatch_err())
        }
    }
}

impl GitBlob {
    pub fn to_bytes(&self) -> Vec<u8> {
        GitObjRaw::from(self.clone()).to_bytes()
    }

    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        let raw = GitObjRaw::from_bytes(bytes, repo)?;
        if raw.typ == GitObjType::Blob {
            Ok(GitBlob {
                data: raw.data,
                repo: raw.repo,
            })
        } else {
            Err(obj_mismatch_err())
        }
    }
}

impl GitCommit {
    /// Serialize commit to bytes
    pub fn to_bytes(&self) -> Vec<u8> {
        GitObjRaw::from(self.clone()).to_bytes()
    }

    /// Deserialize commit from bytes
    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        GitCommit::try_from(GitObjRaw::from_bytes(bytes, repo)?)
    }

    // TODO: Reduce boilerplate
    pub fn read_in_repo(repo: &Path, hash: &GitHash) -> io::Result<Self> {
        Self::try_from(GitObj::read_in_repo(repo, hash)?)
    }

    /// Try to get the parent of a commit in a given repository.
    ///
    /// Return values:
    /// * `None` - no parent
    /// * `Some(Err(_))` - error getting parent
    /// * `Some(Ok(_))` parent obtained successfully
    pub fn parent_in_repo(&self, repo: &Path) -> Option<io::Result<Self>> {
        let hash = self.data.map.get("parent")?;
        match GitHash::from_str(hash) {
            Ok(hash) => Some(Self::read_in_repo(repo, &hash)),
            Err(_) => Some(Err(invalid_data_err("hash error"))), // TODO: boilerplate?
        }
    }

    /// Get `HEAD` commit in a repository
    pub fn head_in_repo(repo: &Path) -> io::Result<Self> {
        let obj = GitRef::read_head_in_repo(repo)?.telescope()?;

        if let GitObj::Commit(commit) = obj {
            Ok(commit)
        } else {
            Err(obj_mismatch_err())
        }
    }

    /// Like `GitCommit::head_in_repo()`, but locate the repository
    /// with `find_repo()`.
    pub fn head() -> io::Result<Self> {
        Self::head_in_repo(&find_repo()?)
    }

    /// Get the commit's tree
    pub fn tree(&self) -> io::Result<GitTree> {
        let hash = self
            .data
            .map
            .get("tree")
            .ok_or_else(|| invalid_data_err("commit has no tree"))?;
        let hash = GitHash::from_str(hash).map_err(|_| invalid_data_err("hash error"))?; // TODO: Remove boilerplate
        let obj = GitObj::read_in_repo(&self.repo, &hash)?;
        let tree = GitTree::try_from(GitObjRaw::from(obj))?;
        Ok(tree)
    }

    /// Update files in the working tree to the time of a commit. This
    /// corresponds to `git checkout`.
    pub fn checkout(&self) -> io::Result<()> {
        self.tree()?.checkout()
    }
}

impl From<GitCommit> for GitObjRaw {
    fn from(obj: GitCommit) -> Self {
        Self {
            typ: GitObjType::Commit,
            data: obj.data.to_bytes(),
            repo: obj.repo,
        }
    }
}

impl TryFrom<GitObjRaw> for GitCommit {
    type Error = io::Error;

    fn try_from(obj: GitObjRaw) -> io::Result<Self> {
        if obj.typ == GitObjType::Commit {
            Ok(GitCommit {
                data: GitKeyValMsg::from_bytes(&obj.data)?,
                repo: obj.repo,
            })
        } else {
            Err(obj_mismatch_err())
        }
    }
}

impl TryFrom<GitObj> for GitCommit {
    type Error = io::Error;

    fn try_from(obj: GitObj) -> io::Result<Self> {
        if let GitObj::Commit(commit) = obj {
            Ok(commit)
        } else {
            Err(obj_mismatch_err())
        }
    }
}

impl From<GitTag> for GitObjRaw {
    fn from(obj: GitTag) -> Self {
        Self {
            typ: GitObjType::Commit,
            data: obj.data.to_bytes(),
            repo: obj.repo,
        }
    }
}

impl TryFrom<GitObjRaw> for GitTag {
    type Error = io::Error;

    fn try_from(obj: GitObjRaw) -> io::Result<Self> {
        if obj.typ == GitObjType::Commit {
            Ok(GitTag {
                data: GitKeyValMsg::from_bytes(&obj.data)?,
                repo: obj.repo,
            })
        } else {
            Err(obj_mismatch_err())
        }
    }
}

impl GitTag {
    pub fn to_bytes(&self) -> Vec<u8> {
        GitObjRaw::from(self.clone()).to_bytes()
    }

    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        GitTag::try_from(GitObjRaw::from_bytes(bytes, repo)?)
    }
}

impl From<GitTree> for GitObjRaw {
    fn from(obj: GitTree) -> Self {
        Self {
            typ: GitObjType::Tree,
            data: obj
                .nodes
                .iter()
                .map(GitTreeNode::to_bytes)
                .flatten()
                .collect::<Vec<u8>>(),
            repo: obj.repo,
        }
    }
}

impl TryFrom<GitObjRaw> for GitTree {
    type Error = io::Error;

    fn try_from(obj: GitObjRaw) -> io::Result<Self> {
        if obj.typ == GitObjType::Tree {
            Ok(GitTree {
                nodes: GitTreeNode::from_bytes_nodes(&obj.data)?,
                repo: obj.repo,
            })
        } else {
            Err(obj_mismatch_err())
        }
    }
}

impl GitTree {
    pub fn to_bytes(&self) -> Vec<u8> {
        GitObjRaw::from(self.clone()).to_bytes()
    }

    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        Self::try_from(GitObjRaw::from_bytes(bytes, repo)?)
    }

    /// Return an iterator over all nodes of the tree
    pub fn walk(&self) -> WalkTree {
        WalkTree {
            item_stack: self
                .nodes
                .iter()
                .map(|node| WalkTreeItem {
                    node: node.clone(),
                    parent_path: PathBuf::from("."), // TODO: Is this okay?
                    repo: self.repo.clone(),
                })
                .collect(),
        }
    }

    /// Determines whether the working tree matches this tree
    pub fn working_same(&self) -> io::Result<bool> {
        self.walk()
            .map(|walk_item| {
                let walk_item = walk_item?;
                let path = walk_item.path();
                let hash = walk_item.node.hash;
                let obj = GitObj::read_in_repo(&self.repo, &hash)?;
                match obj {
                    GitObj::Blob(blob) => Ok(fs::read(path)? == blob.data),
                    GitObj::Tree(tree) => tree.working_same(),
                    _ => Err(obj_mismatch_err()),
                }
            })
            .fold_results(true, |a, b| a && b)
    }

    /// Like `GitTree::checkout()`, but without checking the working
    /// tree.
    fn do_checkout(&self) -> io::Result<()> {
        unimplemented!()
    }

    /// Modify the working tree to fit this tree, doing checks to
    /// prevent data loss. This function corresponds to
    /// `git checkout`.
    // TODO: Fix the check
    pub fn checkout(&self) -> io::Result<()> {
        if self.working_same()? {
            self.do_checkout()
        } else {
            Err(invalid_data_err("working tree dirty"))
        }
    }
}

impl GitObjRaw {
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
                typ: GitObjType::try_from(typ)?,
                data: data.to_vec(),
                repo: repo.to_path_buf(),
            })
        } else {
            Err(invalid_data_err("git object size corrupted"))
        }
    }
}

impl TryFrom<GitObjRaw> for GitObj {
    type Error = io::Error;

    fn try_from(raw: GitObjRaw) -> io::Result<Self> {
        match raw.typ {
            GitObjType::Blob => GitBlob::try_from(raw).map(|o| GitObj::Blob(o)),
            GitObjType::Commit => GitCommit::try_from(raw).map(|o| GitObj::Commit(o)),
            GitObjType::Tag => GitTag::try_from(raw).map(|o| GitObj::Tag(o)),
            GitObjType::Tree => GitTree::try_from(raw).map(|o| GitObj::Tree(o)),
        }
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
pub fn hash_object(path: &Path, typ: GitObjType, do_write: bool) -> io::Result<GitHash> {
    let obj = GitObjRaw {
        typ,
        data: fs::read(path)?,
        repo: find_repo()?,
    };
    let obj = GitObj::try_from(obj)?;
    if do_write {
        obj.write()?;
    }
    Ok(obj.hash())
}

/// Find the path of a git object from its hash in a given
/// repository. This always succeeds, since it does not check if
/// the object exists.
pub fn find_object_in_repo(repo: &Path, hash: &GitHash) -> PathBuf {
    let objs = repo.join(GIT_DIR.to_os_string()).join("objects");
    let hash = hash.to_string();
    let (obj_dir, obj_file) = hash.split_at(2);

    objs.join(obj_dir).join(obj_file)
}

/// Like `find_object_in_repo()`, but locates the repository with
/// `find_repo()`
pub fn find_object(hash: &GitHash) -> io::Result<PathBuf> {
    Ok(find_object_in_repo(&find_repo()?, hash))
}
