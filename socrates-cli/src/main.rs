use std::cell::RefCell;
use std::fs::File;
use std::io::{stdin, stdout, Read};
use std::rc::Rc;

use ::minisat::Bool;
use directories::ProjectDirs;
use fnv::FnvHashMap;

mod minisat;

use crate::minisat::MinisatReceiver;
use socrates_ast::parsed::{InteractiveCommand, InteractiveItem};
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

    let project_dirs = ProjectDirs::from("io.github", "mhallin", "socrates");
    let history_file = project_dirs
        .as_ref()
        .map(|p| p.data_dir().join("history.txt"));
    match &history_file {
        Some(history_file) => {
            if history_file.exists() {
                if let Err(err) = editor.load_history(&history_file) {
                    log::warn!("Could not load history {:?}", err);
                }
            }
        }
        None => log::warn!(
            "Could not load history, the application data folder could not be determined"
        ),
    }

    println!("Socrates, a multi-purpose first-order reasoning tool");
    println!();

    let mut storage = TypeStorage::new();
    let mut buckets = FnvHashMap::default();
    let gafs = GAFStorage::new();
    let solver = Rc::new(RefCell::new(::minisat::Solver::new()));
    let receiver = MinisatReceiver::new(Rc::clone(&solver));
    let mut emitter = ToplevelCNFEmitter::new(&gafs, &receiver);

    let result = loop {
        match editor.readline(">>> ") {
            Ok(line) => {
                let line: &'static str = Box::leak(line.to_owned().into_boxed_str());
                let mut errors = ErrorContext::new("<interactive>", &line);
                match SingleInteractiveItemParser::new().parse(&mut errors, &line) {
                    Ok(InteractiveItem::Item(item)) => {
                        if !handle_item(
                            &mut emitter,
                            *item,
                            &mut storage,
                            &mut errors,
                            &mut buckets,
                        ) {
                            log::warn!("handle_item failed");
                        }
                    }
                    Ok(InteractiveItem::Interactive(item)) => {
                        log::info!("Interactive item {:?}", item);
                        match item.inner {
                            InteractiveCommand::ShowModel => {
                                let mut solver = solver.borrow_mut();
                                match solver.solve() {
                                    Ok(model) => {
                                        println!("SAT");
                                        gafs.with_gafs(|gafs| {
                                            for (gaf, index) in gafs {
                                                if let Some(literal) =
                                                    receiver.literal(index.0 as usize)
                                                {
                                                    let value = model.value(&Bool::Lit(literal));
                                                    println!(
                                                        "  {}{}",
                                                        if value { "" } else { "~" },
                                                        gaf
                                                    );
                                                }
                                            }
                                        });
                                    }
                                    Err(()) => {
                                        println!("UNSAT");
                                    }
                                }
                            }
                            InteractiveCommand::ProveFormula(_) => todo!(),
                        }
                    }
                    Err(e) => errors.push_error(WrappedLalrpopError(e)),
                }
                editor.add_history_entry(line);
                errors.write_user_friendly(&mut stdout())?;
            }
            Err(rustyline::error::ReadlineError::Eof) => {
                break Ok(());
            }
            Err(rustyline::error::ReadlineError::Interrupted) => {
                continue;
            }
            Err(e) => {
                break Err(e.into());
            }
        }
    };

    emitter.finish()?;

    if let (Some(history_file), Some(project_dirs)) = (&history_file, &project_dirs) {
        let mut can_write = true;

        if !project_dirs.data_dir().exists() {
            if let Err(err) = std::fs::create_dir_all(project_dirs.data_dir()) {
                log::warn!("Could not save history file: {:?}", err);
                can_write = false;
            }
        }

        if can_write {
            if let Err(err) = editor.save_history(history_file) {
                log::warn!("Could not save history file: {:?}", err);
            }
        }
    };

    result
}
