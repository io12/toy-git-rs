#[macro_use]
extern crate lazy_static;
extern crate serde_ini;

use std::env;
use std::ffi::OsString;
use std::fs;
use std::io;
use std::path::Path;

lazy_static! {
    static ref GIT_DIR: OsString = env::var_os("GIT_DIR").unwrap_or_else(|| OsString::from(".git"));
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

    fs::File::create(git_dir.join("config"))?; // TODO: write contents
    fs::File::create(git_dir.join("description"))?; // TODO: write contents
    fs::File::create(git_dir.join("HEAD"))?; // TODO: write contents

    Ok(())
}
