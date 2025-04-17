use std::fs;
use std::path::PathBuf;
use std::fmt::Debug;

use clap::Parser;

mod cpu;

mod lexer;
use lexer::Token;

mod section;
use section::{Section, SymbolRegistry};

//mod templater;

#[derive(Debug, Parser)]
struct Args {
    #[arg(short = 'v')]
    verbose: bool,

    file: PathBuf,
}

fn main() {
    let args = Args::parse();

    let src = fs::read_to_string(args.file).unwrap();
    //let f = templater::process(&f);
    let (tokens, _) = lexer::parse(&src);
    let tokens = tokens.unwrap();

    let mut reg = SymbolRegistry::new();

    for t in tokens.iter() {
        match &t {
            Token::SetStr(name, value) => reg.insert_str(name, value),
            Token::SetNum(name, value) => reg.insert_num(name, *value),
            Token::Label(name) => reg.import(name),
            _ => (),
        }
    }

    println!("symbols: {:?}", reg);

    let mut sections = Vec::new();
    let mut cur_section = None;
    use cpu::EncodeInstruction;
    for t in tokens.iter() {
        match t {
            Token::Section(name, base) => {
                if let Some(s) = cur_section.take() {
                    sections.push(s);
                }
                let base = base.map(|v| v as usize);
                cur_section = Some(Section::new(name, base));
            },
            Token::Label(l) => {
                if let Some(s) = cur_section.as_mut() {
                    s.define_label(l);
                } else {
                    eprintln!("no section but tried to define a label");
                    todo!("error handling");
                }
            },
            Token::Instruction(i) => {
                if let Some(s) = cur_section.as_mut() {
                    i.encode(s, &mut reg);
                } else {
                    eprintln!("no section but tried to emit instruction");
                    todo!("error handling");
                }
            },
            _ => (),
        }
    }
    if let Some(s) = cur_section.take() {
        sections.push(s);
    }


    println!("{:#x?}", sections);

/*    println!("CPU: {cpu:?}");
    println!("OS/ABI: {os:?}");
    println!("Constant symbols:");
    println!("{:#x?}", const_symbols);
    println!("Imported symbols:");
    println!("{:#x?}", imports);
    println!("Exported symbols:");
    println!("{:#x?}", exports);
    println!("Labels:");
    println!("{:#x?}", labels);
    println!("Sections:");
    println!("{:#x?}", sections);*/

}
