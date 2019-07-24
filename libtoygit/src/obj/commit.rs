use crate::*;

use std::fmt;

use chrono::prelude::*;

/// A git commit object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Commit {
    /// Commit data
    pub data: KeyValMsg,
    /// Path to repository
    pub repo: PathBuf,
}

/// Type representing the data held by the `author` and `committer` fields of a
/// commit
struct CommitAuthorField {
    /// The name and email, in the format "John Doe <email@example.com>"
    name_and_email: String,
    /// The time of this commit
    time: NaiveDateTime,
    /// I have no idea what this is for. Please tell me if you know.
    other: String,
}

impl Commit {
    /// Serialize commit to bytes
    pub fn to_bytes(&self) -> Vec<u8> {
        obj::Raw::from(self.clone()).to_bytes()
    }

    /// Deserialize commit from bytes
    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        Commit::try_from(obj::Raw::from_bytes(bytes, repo)?)
    }

    // TODO: Reduce boilerplate
    pub fn read_in_repo(repo: &Path, hash: &Hash) -> io::Result<Self> {
        Self::try_from(Obj::read_in_repo(repo, hash)?)
    }

    /// Try to get the parent of a commit in a given repository.
    ///
    /// Return values:
    /// * `None` - no parent
    /// * `Some(Err(_))` - error getting parent
    /// * `Some(Ok(_))` parent obtained successfully
    pub fn parent_in_repo(&self, repo: &Path) -> Option<io::Result<Self>> {
        let hash = self.data.map.get("parent")?;
        match Hash::from_str(hash) {
            Ok(hash) => Some(Self::read_in_repo(repo, &hash)),
            Err(_) => Some(Err(invalid_data_err("hash error"))), // TODO: boilerplate?
        }
    }

    /// Get `HEAD` commit in a repository
    pub fn head_in_repo(repo: &Path) -> io::Result<Self> {
        let obj = Ref::read_head_in_repo(repo)?.telescope()?;

        if let Obj::Commit(commit) = obj {
            Ok(commit)
        } else {
            Err(obj_mismatch_err())
        }
    }

    /// Like `Commit::head_in_repo()`, but locate the repository
    /// with `find_repo()`.
    pub fn head() -> io::Result<Self> {
        Self::head_in_repo(&find_repo()?)
    }

    /// Get the commit's tree
    pub fn tree(&self) -> io::Result<obj::Tree> {
        let hash = self
            .data
            .map
            .get("tree")
            .ok_or_else(|| invalid_data_err("commit has no tree"))?;
        let hash = Hash::from_str(hash).map_err(|_| invalid_data_err("hash error"))?; // TODO: Remove boilerplate
        let obj = Obj::read_in_repo(&self.repo, &hash)?;
        let tree = obj::Tree::try_from(obj::Raw::from(obj))?;
        Ok(tree)
    }

    /// Update files in the working tree to the time of a commit. This
    /// corresponds to `git checkout`.
    pub fn checkout(&self) -> io::Result<()> {
        self.tree()?.checkout()
    }

    /// Get the hash of a commit object
    // TODO: try to reduce boilerplate somehow
    pub fn hash(&self) -> Hash {
        Obj::Commit(self.to_owned()).hash()
    }

    /// Retrieve and parse the `author` field
    fn author_field(&self) -> io::Result<CommitAuthorField> {
        self.data
            .map
            .get("author")
            .ok_or_else(|| invalid_data_err("commit has no author"))?
            .parse()
    }
}

impl CommitAuthorField {
    fn error() -> io::Error {
        invalid_data_err("invalid commit author field")
    }
}

impl FromStr for CommitAuthorField {
    type Err = io::Error;

    fn from_str(s: &str) -> io::Result<Self> {
        let split = s.find("> ").ok_or_else(|| Self::error())?;
        let split = split + 1;
        let (name_and_email, time_other) = s.split_at(split);
        let (time, other) = time_other
            .trim()
            .split_whitespace()
            .next_tuple()
            .ok_or_else(|| Self::error())?;
        let time = time.parse::<i64>().map_err(|_| Self::error())?;
        let time = NaiveDateTime::from_timestamp_opt(time, 0).ok_or_else(|| Self::error())?;
        Ok(Self {
            name_and_email: name_and_email.to_string(),
            time,
            other: other.to_string(),
        })
    }
}

impl fmt::Display for Commit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "commit {}", self.hash())?;
        if let Ok(author) = self.author_field() {
            writeln!(f, "Author: {}", author.name_and_email)?;
            writeln!(f, "Date:   {} {}", author.time, author.other)?;
        }
        writeln!(f)?;
        writeln!(f, "    {}", self.data.msg)?;
        Ok(())
    }
}
