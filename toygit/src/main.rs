extern crate libtoygit;
#[macro_use]
extern crate clap;
#[macro_use]
extern crate failure;

use std::path::Path;

use libtoygit as git;

use clap::{AppSettings, Arg, ArgMatches, SubCommand};
use failure::Fallible;

fn git_init(args: &ArgMatches) -> Fallible<()> {
    let dir = args
        .value_of_os("directory")
        .map(Path::new)
        .unwrap_or(Path::new("."));

    git::init(dir)?;
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
}

fn try_main() -> Fallible<()> {
    let args = make_clap_app().get_matches();

    match args.subcommand() {
        ("init", Some(args)) => git_init(args),
        _ => bail!("unrecognized subcommand"),
    }
}

fn main() {
    if let Err(err) = try_main() {
        eprintln!("git: {}", err);
        std::process::exit(1);
    }
}
