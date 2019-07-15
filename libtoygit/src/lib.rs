#[macro_use]
extern crate lazy_static;
extern crate tini;

use std::env;
use std::ffi::OsString;
use std::fs;
use std::io;
use std::path::Path;

use tini::Ini;

const DEFAULT_DESCRIPTION: &str =
    "Unnamed repository; edit this file 'description' to name the repository.\n";

lazy_static! {
    static ref GIT_DIR: OsString = env::var_os("GIT_DIR").unwrap_or_else(|| OsString::from(".git"));
    static ref DEFAULT_CONFIG: Ini = Ini::new()
        .section("core")
        .item("repositoryformatversion", "0")
        .item("filemode", "true")
        .item("bare", "false")
        .item("logallrefupdates", "true");
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
    fs::File::create(git_dir.join("HEAD"))?; // TODO: write contents

    Ok(())
}
