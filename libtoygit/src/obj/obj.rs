use crate::*;

use std::fs::File;

use libflate::zlib;

/// A generic git object
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Obj {
    Blob(obj::Blob),
    Commit(obj::Commit),
    Tag(obj::Tag),
    Tree(obj::Tree),
}

impl Obj {
    /// Read the git object with hash `hash` in repo `repo`
    pub fn read_in_repo(repo: &Path, hash: &Hash) -> io::Result<Self> {
        let path = find_object_in_repo(repo, hash);
        debug!("opening file {:?}", path);
        let file = File::open(path)?;

        // Read bytes of decompressed object
        let bytes = zlib::Decoder::new(file)?
            .bytes()
            .collect::<io::Result<Vec<u8>>>()?;

        Self::from_bytes(&bytes, repo)
    }

    /// Like `Obj::read_in_repo()`, but locates the repository with
    /// `find_repo()`
    pub fn read(hash: &Hash) -> io::Result<Self> {
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

    /// Like `Obj::write_in_repo()`, but locates the repository
    /// with `find_repo()`
    pub fn write(&self) -> io::Result<()> {
        self.write_in_repo(&find_repo()?)
    }

    /// Compute the hash of an object
    pub fn hash(&self) -> Hash {
        let bytes = self.to_bytes();
        Hasher::from(bytes).digest()
    }

    /// Convert to `obj::Raw`
    fn raw(&self) -> obj::Raw {
        obj::Raw::from(self.clone())
    }

    /// Get the type of an object
    pub fn typ(&self) -> obj::Kind {
        self.raw().typ
    }

    /// Get the raw data of an object
    pub fn data(&self) -> Vec<u8> {
        self.raw().data
    }
}

impl Obj {
    pub fn to_bytes(&self) -> Vec<u8> {
        obj::Raw::from(self.clone()).to_bytes()
    }

    pub fn from_bytes(bytes: &[u8], repo: &Path) -> io::Result<Self> {
        let raw = obj::Raw::from_bytes(bytes, repo)?;
        let obj = Obj::try_from(raw)?;
        Ok(obj)
    }
}
