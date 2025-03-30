use object::{Object, ObjectSection};

use clap::Parser;
use std::fs;
use std::path::PathBuf;

#[derive(Debug, Parser)]
struct Args {
    input: PathBuf,
    output: PathBuf,
}

fn main() {
    let args = Args::parse();
    let data = fs::read(args.input).unwrap();
    let inp = object::File::parse(&*data).unwrap();

    for s in inp.sections() {
        println!("{:x?}", s);
    }
}
