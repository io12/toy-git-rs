extern crate libtoygit;
#[macro_use]
extern crate clap;
extern crate string_error;

use std::boxed::Box;
use std::error::Error;
use std::io;
use std::io::Write;
use std::path::Path;

use libtoygit::ObjHash as GitHash;

use clap::{AppSettings, Arg, ArgMatches, SubCommand};

type Result<T> = std::result::Result<T, Box<Error>>;

fn git_init(args: &ArgMatches) -> Result<()> {
    let dir = args
        .value_of_os("directory")
        .map(Path::new)
        .unwrap_or(Path::new("."));

    libtoygit::init(dir)?;
    Ok(())
}

fn git_cat_file(args: &ArgMatches) -> Result<()> {
    let hash = args.value_of("object").expect("no object");
    let hash = hash.parse::<GitHash>()?;
    let obj = libtoygit::Obj::read(&hash)?;
    io::stdout().write_all(&obj.data)?;
    Ok(())
}

fn make_clap_app() -> clap::App<'static, 'static> {
    app_from_crate!()
        .setting(AppSettings::ArgRequiredElseHelp)
        .subcommand(
            SubCommand::with_name("init")
                .about("Create an empty Git repository or reinitialize an existing one")
                .arg(Arg::with_name("directory")),
        )
        .subcommand(
            SubCommand::with_name("cat-file")
                .about("Provide content or type and size information for repository objects")
                .arg(Arg::with_name("object").required(true)),
        )
}

// Convenience wrapper function around `string_error`
fn error(msg: &'static str) -> Result<()> {
    Err(string_error::static_err(msg))
}

fn try_main() -> Result<()> {
    let args = make_clap_app().get_matches();

    match args.subcommand() {
        ("init", Some(args)) => git_init(args),
        ("cat-file", Some(args)) => git_cat_file(args),
        _ => error("unrecognized subcommand"),
    }
}

fn main() {
    if let Err(err) = try_main() {
        eprintln!("git: {}", err);
        std::process::exit(1);
    }
}
