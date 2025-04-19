use std::fs;
use std::path::PathBuf;
use std::fmt::Debug;

use annotate_snippets::{Level, Renderer, Snippet};
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
    let filename = args.file
        .display()
        .to_string();

    let src = fs::read_to_string(&args.file).unwrap();
    let src = src.replace('\t', "        ");
    //let f = templater::process(&f);
    let (mut tokens, _) = lexer::parse(&src);
    let mut tokens = match tokens {
        Ok(t) => t,
        Err(e) => {
            eprintln!("raw {:#?}", &e);
            let e = e.into_inner().unwrap();

            let mut annotations = Vec::new();
            for c in e.context() {
                use lexer::ParseContext;
                match c {
                    ParseContext::CurrentCpu(spanned) => {
                        let cpu = spanned.inner();
                        let span = spanned.span();
                        let label = format!("Current CPU is {:?}", cpu).leak();
                        let msg = Level::Note
                            .span(span.clone())
                            .label(label);
                        annotations.push(msg);
                    },
                    ParseContext::Annotation(level, span, message) => {
                        let label = message.clone().leak();
                        annotations.push(level.span(span.clone()).label(message));
                    },
                    ParseContext::Label(lab) => {
                    },
                }
            }

            let msg = Level::Error.title("Parsing failed")
                .snippet(
                    Snippet::source(&src)
                    .origin(&filename)
                    .fold(true)
                    .annotations(annotations)
                );

            let renderer = Renderer::styled();
            anstream::println!("{}", renderer.render(msg));

            return
        },
    };

    let mut reg = SymbolRegistry::new();

    for t in tokens.iter_mut() {
        match t {
            Token::SetStr(name, value) => reg.insert_str(name, value),
            Token::SetNum(name, value) => reg.insert_num(name, *value),
            Token::Label(name) => reg.import(name),
            Token::Error(ref mut err) => {
                let footers = err.take_annotations();
                let label = err.header();
                let span = err.span();
                let msg = Level::Error.title(label)
                    .snippet(
                        Snippet::source(&src)
                            .origin(&filename)
                            .fold(true)
                            .annotation(Level::Info
                                .span(span.clone()))
                    )
                    .footers(footers);

                let renderer = Renderer::styled();
                anstream::println!("{}", renderer.render(msg));

            },
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
