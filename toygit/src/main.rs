extern crate libtoygit;
#[macro_use]
extern crate clap;
extern crate string_error;

use std::boxed::Box;
use std::error::Error;
use std::io::{self, Write};
use std::path::Path;

use libtoygit::{GitHash, GitObj, GitObjType};

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
    let typ = args
        .value_of("type")
        .expect("no type")
        .parse::<GitObjType>()?;
    let hash = args
        .value_of("object")
        .expect("no object")
        .parse::<GitHash>()?;

    let obj = GitObj::read(&hash)?;

    if obj.typ == typ {
        io::stdout().write_all(&obj.data)?;
        Ok(())
    } else {
        error("object does not have specified type")
    }
}

fn git_hash_object(args: &ArgMatches) -> Result<()> {
    let do_write = args.is_present("do-write");
    let typ = args
        .value_of("type")
        .expect("no type")
        .parse::<GitObjType>()?;
    let path = args.value_of_os("file").expect("no file");
    let path = Path::new(path);

    let hash = libtoygit::hash_object(path, typ, do_write)?;

    println!("{}", hash);
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
                .arg(
                    Arg::with_name("type")
                        .required(true)
                        .possible_values(&["blob", "commit", "tag", "tree"]),
                )
                .arg(Arg::with_name("object").required(true)),
        )
        .subcommand(
            SubCommand::with_name("hash-object")
                .about("Compute object ID and optionally creates a blob from a file")
                .arg(
                    Arg::with_name("do-write")
                        .short("w")
                        .help("Actually write the object into the object database"),
                )
                .arg(
                    Arg::with_name("type")
                        .short("t")
                        .value_name("type")
                        .help("Specify the type")
                        .default_value("blob"),
                )
                .arg(Arg::with_name("file").required(true)),
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
        ("hash-object", Some(args)) => git_hash_object(args),
        _ => error("unrecognized subcommand"),
    }
}

fn main() {
    if let Err(err) = try_main() {
        eprintln!("git: {}", err);
        std::process::exit(1);
    }
}
