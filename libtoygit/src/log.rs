use crate::*;

use obj::Obj;

use std::io;

/// An iterator (log) of commits. This corresponds to `git log`.
#[derive(Debug)]
pub struct Log {
    /// Hash of next commit
    next_commit_hash: Option<io::Result<Hash>>,
    /// Path to repository
    repo: PathBuf,
}

impl Log {
    /// Get an iterator over a repository's commits from newest to
    /// oldest, beginning from `hash`.
    pub fn from_hash_repo(hash: &Hash, repo: &Path) -> Self {
        Self {
            next_commit_hash: Some(Ok(*hash)),
            repo: repo.to_path_buf(),
        }
    }

    /// Like `Log::from_hash_repo()`, but locate the repository
    /// with `find_repo()`
    pub fn from_hash(hash: &Hash) -> io::Result<Self> {
        Ok(Self::from_hash_repo(hash, &find_repo()?))
    }

    /// Like `Log::from_hash_repo()`, but with `HEAD` as the commit
    /// hash.
    pub fn from_repo(repo: &Path) -> io::Result<Self> {
        let commit = obj::Commit::head_in_repo(repo)?;
        let obj = Obj::Commit(commit);
        let hash = obj.hash();

        Ok(Self::from_hash_repo(&hash, repo))
    }

    /// Like `Log::from_hash_repo()`, but with `HEAD` as the commit
    /// hash and locate the repository with `find_repo()`
    pub fn new() -> io::Result<Self> {
        let repo = find_repo()?;

        Self::from_repo(&repo)
    }
}

impl Iterator for Log {
    type Item = io::Result<obj::Commit>;

    // TODO: Refactor
    fn next(&mut self) -> Option<Self::Item> {
        match &self.next_commit_hash {
            Some(Ok(hash)) => {
                let obj = Obj::read_in_repo(&self.repo, &hash);
                let commit = obj.and_then(|o| obj::Commit::try_from(o));
                self.next_commit_hash = match commit {
                    Ok(ref commit) => match commit.parent_in_repo(&self.repo) {
                        Some(Ok(commit)) => Some(Ok(Obj::Commit(commit).hash())),
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
