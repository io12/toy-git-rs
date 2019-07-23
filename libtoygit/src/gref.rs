use crate::*;
use obj::Obj;

use std::io;

/// A git reference. This is the parsed version of a text file which
/// can either be a SHA1 hash or a path to another reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ref {
    Hash {
        /// Repository containing the reference
        repo: PathBuf,
        /// An object hash
        hash: Hash,
    },
    Ref {
        /// Repository containing the reference
        repo: PathBuf,
        /// Path to next reference
        path: PathBuf,
    },
}

impl Ref {
    fn error() -> io::Error {
        invalid_data_err("failed to parse ref")
    }

    /// Recursively follow reference, yielding target object
    pub fn telescope(&self) -> io::Result<Obj> {
        match self {
            Ref::Hash { hash, repo } => Obj::read_in_repo(&repo, &hash),
            Ref::Ref { path, repo } => Ref::read_in_repo(&repo, &path)?.telescope(),
        }
    }

    /// Read a git reference from a file in a repository
    pub fn read_in_repo(repo: &Path, path: &Path) -> io::Result<Self> {
        let git_dir = GIT_DIR.to_os_string();
        let path = repo.join(git_dir).join(path);

        debug!("reading file: {:?}", path);
        Self::from_bytes(&fs::read(path)?, repo)
    }

    /// Like `Ref::read_in_repo()`, but locate the repo with
    /// `find_repo()`
    pub fn read(path: &Path) -> io::Result<Self> {
        let repo = find_repo()?;
        Self::read_in_repo(&repo, path)
    }

    /// Like `Ref::read_head_in_repo()`, but finds repository with
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

    pub fn from_str(s: &str, repo: &Path) -> io::Result<Self> {
        let repo = repo.to_path_buf();

        match s.split(' ').next_tuple() {
            Some(("ref:", path)) => Ok(Ref::Ref {
                path: PathBuf::from(path.trim()),
                repo,
            }),
            _ => {
                let hash = s.trim();
                let hash = Hash::from_str(hash);
                let hash = hash.map_err(|_| Ref::error())?;
                let hash = Ref::Hash { hash, repo };
                Ok(hash)
            }
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.to_string().bytes().collect()
    }

    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        let res = str::from_utf8(bytes);
        let res = res.map_err(|_| Ref::error());
        let s = res?;
        Self::from_str(s, repo)
    }
}

impl fmt::Display for Ref {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ref::Hash { hash, .. } => write!(f, "{}", hash),
            Ref::Ref { path, .. } => write!(f, "{}", path.display()),
        }
    }
}
