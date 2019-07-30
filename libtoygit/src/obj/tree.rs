use crate::*;
use obj::Obj;

use std::ffi::OsStr;
use std::fs;
use std::iter;
use std::os::unix::fs::PermissionsExt;

use walkdir::WalkDir;

/// Node of a git tree object. This represents a file or directory in
/// a tree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    /// Permission bits
    pub mode: fs::Permissions,
    /// Name of file or directory
    pub filename: OsString,
    /// Hash of a git object, which can be either a tree (for
    /// directories) or a blob (for files)
    pub hash: Hash,
}

/// A git tree object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tree {
    /// Nodes of the tree
    pub nodes: Vec<Node>,
    /// Path to repository
    pub repo: PathBuf,
}

/// An iterator over nodes of a git tree.
#[derive(Debug)]
pub struct WalkTree {
    item_stack: Vec<WalkTreeItem>,
}

/// Type returned by iterating over `WalkTree`. It contains a
/// `Node` and the path to its parent.
#[derive(Debug)]
pub struct WalkTreeItem {
    /// Tree node
    pub node: Node,
    /// Path to node's parent relative to the repository. Note that
    /// `Node` only stores the filename, which is why this is
    /// needed.
    pub parent_path: PathBuf,
    /// Path to repository
    pub repo: PathBuf,
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
        // repo/parent_path/filename
        self.repo.join(self.parent_path.join(filename))
    }

    /// Get a list of the node's children. If this node is not a
    /// directory, then the returned vector is empty.
    pub fn children(&self) -> io::Result<Vec<Self>> {
        let obj = Obj::read_in_repo(&self.repo, &self.node.hash)?;

        match obj {
            // Node is file
            Obj::Blob(_) => Ok(Vec::new()),
            // Node is directory
            Obj::Tree(tree) => Ok(tree
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

impl Node {
    fn error() -> io::Error {
        invalid_data_err("invalid tree node")
    }

    /// Given a byte slice containing an encoded `Node`, find
    /// its size in bytes.
    fn byte_bound(bytes: &[u8]) -> Option<usize> {
        bytes
            .iter()
            .position(|&b| b == b'\x00')
            .map(|n| n + HASH_LEN)
            .filter(|&n| n < bytes.len())
    }

    /// Deserialize a `Node` from bytes, returning the
    /// `Node` and the rest of the bytes
    fn from_bytes_rest<'a>(bytes: &'a [u8]) -> io::Result<(Node, &'a [u8])> {
        let opt_n = Node::byte_bound(bytes);
        match opt_n {
            Some(n) => {
                let (node_bytes, rest) = bytes.split_at(n + 1);
                let node = Node::from_bytes(node_bytes)?;
                Ok((node, rest))
            }
            None => Err(Node::error()),
        }
    }

    /// Deserialize `Node`s from a byte slice
    pub fn from_bytes_nodes(mut bytes: &[u8]) -> io::Result<Vec<Self>> {
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
        let toks = bytes.splitn(3, |&b| b == b' ' || b == b'\x00').next_tuple();
        match toks {
            Some((mode, filename, hash)) => {
                // TODO: Refactor
                let mode = str::from_utf8(mode);
                let mode = mode.map_err(|_| Self::error())?;
                let mode = mode.parse::<u32>();
                let mode = mode.map_err(|_| Self::error())?;
                let mode = fs::Permissions::from_mode(mode);

                let filename = OsStr::from_bytes(filename).to_os_string();

                let hash = hex::encode(hash);
                let hash = Hash::from_str(&hash);
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

impl Tree {
    pub fn to_bytes(&self) -> Vec<u8> {
        obj::Raw::from(self.clone()).to_bytes()
    }

    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        Self::try_from(obj::Raw::from_bytes(bytes, repo)?)
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
                let obj = Obj::read_in_repo(&self.repo, &hash)?;
                match obj {
                    Obj::Blob(blob) => Ok(match fs::read(&path) {
                        Ok(data) => data == blob.data,
                        Err(_) => {
                            debug!("working tree difference at '{}'", path.display());
                            false
                        }
                    }),
                    Obj::Tree(tree) => tree.working_same(),
                    _ => Err(obj_mismatch_err()),
                }
            })
            .fold_results(true, |a, b| a && b)
    }

    /// Like `Tree::checkout()`, but without checking the working
    /// tree.
    fn do_checkout(&self) -> io::Result<()> {
        let repo = &self.repo;

        // Remove all files in the working tree
        let w = WalkDir::new(repo)
            .into_iter()
            .filter_entry(|ent| ent.file_name() != *GIT_DIR);
        for ent in w {
            let ent = ent?;
            let path = ent.path();

            debug!("removing file '{}'", path.display());
            fs::remove_file(path)?;
        }
        Ok(())
    }

    /// Modify the working tree to fit this tree, doing checks to
    /// prevent data loss. This function corresponds to
    /// `git checkout`.
    // TODO: Fix the check
    pub fn checkout(&self) -> io::Result<()> {
        let head = obj::Commit::head_in_repo(&self.repo)?;
        // Whether the working tree is dirty
        let working_dirty = head.tree()?.working_same()?;
        if working_dirty {
            self.do_checkout()
        } else {
            Err(invalid_data_err("working tree dirty"))
        }
    }
}
