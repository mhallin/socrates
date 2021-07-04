use std::fs::File;
use std::io::{stdin, stdout, Read};

use fnv::FnvHashMap;

use socrates_ast::parsed::InteractiveItem;
use socrates_errors::{eyre::Error, ErrorContext};
use socrates_parser::{DocumentParser, SingleInteractiveItemParser, WrappedLalrpopError};

use socrates_core::{
    handle_item, DIMACSReceiver, Emitter, GAFStorage, ToplevelCNFEmitter, TypeStorage,
};

fn main() -> Result<(), Error> {
    env_logger::init();

    if atty::is(atty::Stream::Stdout) {
        run_repl()
    } else {
        let mut stdin = stdin();
        let mut data = String::new();
        stdin.read_to_string(&mut data).unwrap();

        let mut errors = ErrorContext::new("<stdin>", &data);

        let document = match DocumentParser::new().parse(&mut errors, &data) {
            Ok(document) => document,
            Err(e) => {
                errors.push_error(WrappedLalrpopError(e));
                vec![]
            }
        };

        // println!("Document: {:#?}", document);

        let mut storage = TypeStorage::new();
        let mut buckets = FnvHashMap::default();
        let gafs = GAFStorage::new();
        let mut emitter =
            ToplevelCNFEmitter::new(&gafs, DIMACSReceiver::new(File::create("output.dimacs")?)?);

        for item in document {
            if !handle_item(&mut emitter, item, &mut storage, &mut errors, &mut buckets) {
                log::warn!("handle_item failed");
            }
        }

        errors.write_user_friendly(&mut stdout())?;

        emitter.finish()?;

        Ok(())
    }
}

fn run_repl() -> Result<(), Error> {
    let mut editor: rustyline::Editor<()> = rustyline::Editor::new();

    println!("Socrates, a multi-purpose first-order reasoning tool");
    println!();

    let mut storage = TypeStorage::new();
    let mut buckets = FnvHashMap::default();
    let gafs = GAFStorage::new();
    let mut emitter =
        ToplevelCNFEmitter::new(&gafs, DIMACSReceiver::new(File::create("output.dimacs")?)?);

    loop {
        match editor.readline(">>> ") {
            Ok(line) => {
                let line: &'static str = Box::leak(line.to_owned().into_boxed_str());
                let mut errors = ErrorContext::new("<interactive>", &line);
                match SingleInteractiveItemParser::new().parse(&mut errors, &line) {
                    Ok(InteractiveItem::Item(item)) => {
                        if !handle_item(&mut emitter, *item, &mut storage, &mut errors, &mut buckets)
                        {
                            log::warn!("handle_item failed");
                        }
                    }
                    Ok(InteractiveItem::Interactive(item)) => {
                        log::info!("Interactive item {:?}", item)
                    }
                    Err(e) => errors.push_error(WrappedLalrpopError(e)),
                }
                editor.add_history_entry(line);
                errors.write_user_friendly(&mut stdout())?;
            }
            Err(rustyline::error::ReadlineError::Eof) => {
                emitter.finish()?;
                return Ok(());
            }
            Err(e) => {
                emitter.finish()?;
                return Err(e.into());
            }
        }
    }
}
