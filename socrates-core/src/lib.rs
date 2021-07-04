mod cnf;
mod cnf_receiver;
mod gaf;
mod grounder;
mod ident_resolver;
mod predicate_completion;
mod printer;
mod scope;
mod type_inferencer;
mod types;
mod util;

pub use cnf_receiver::DIMACSReceiver;
pub use gaf::GAFStorage;
pub use grounder::{Emitter, ToplevelCNFEmitter};
pub use types::TypeStorage;

#[cfg(test)]
extern crate env_logger;

#[cfg(test)]
mod tests;

use std::collections::HashMap;

use socrates_ast::parsed::{Definition, DocumentItem};
use socrates_ast::Spanning;
use socrates_errors::ErrorContext;

pub fn handle_item<'i, E: grounder::Emitter<'i>, S: ::std::hash::BuildHasher>(
    emitter: &mut E,
    item: Spanning<DocumentItem<'i>>,
    storage: &mut types::TypeStorage<'i>,
    errors: &mut ErrorContext<'i>,
    buckets: &mut HashMap<&'i str, predicate_completion::Bucket, S>,
) -> bool {
    let result = match item.inner {
        DocumentItem::Definition(Spanning {
            inner: Definition::Type(spec),
            ..
        }) => storage.add_uninterpreted_type(&spec, errors),
        DocumentItem::Definition(Spanning {
            inner: Definition::Instances(super_type, name, instances),
            ..
        }) => storage.add_interpreted_type(&name, &super_type, &instances, errors),
        DocumentItem::Definition(Spanning {
            inner: Definition::IntegerType(name, min, max),
            ..
        }) => storage.add_integer_type(&name, &min, &max, errors),
        DocumentItem::Definition(Spanning {
            inner: Definition::Predicate(name, args),
            ..
        }) => storage.add_predicate(&name, &args, errors),
        DocumentItem::Definition(Spanning {
            inner: Definition::UninterpretedFunction(return_type, name, args),
            ..
        }) => storage.add_uninterpreted_function(&name, &return_type, &args, errors),
        DocumentItem::Definition(Spanning {
            inner: Definition::NumericFunction(name, args),
            ..
        }) => storage.add_numeric_function(&name, &args, errors),
        DocumentItem::Definition(Spanning {
            inner: Definition::CompletionBucket(name, _predicate),
            ..
        }) => {
            if buckets.get(name.inner).is_some() {
                errors
                    .push_error_message(name.pos, format!("Bucket {} already exists", name.inner));
                return false;
            }
            buckets.insert(name.inner, predicate_completion::Bucket::new());
            Ok(())
        }
        DocumentItem::BucketedFormula(bucket, formula) => {
            match buckets.get(bucket.inner) {
                Some(_bucket) => (),
                None => {
                    errors.push_error_message(
                        bucket.pos,
                        format!("Bucket {} does not exist", bucket.inner),
                    );
                    return false;
                }
            }

            emit_formula(emitter, formula, storage, errors)
        }
        DocumentItem::Formula(formula) => emit_formula(emitter, formula, storage, errors),
    };

    result.is_ok()
}

fn emit_formula<'i, E: grounder::Emitter<'i>>(
    emitter: &mut E,
    formula: Spanning<socrates_ast::parsed::Formula<'i>>,
    storage: &types::TypeStorage<'i>,
    errors: &mut ErrorContext<'i>,
) -> Result<(), failure::Error> {
    let formula = errors.block_exec(|errors| {
        Ok(ident_resolver::resolve_identifiers(
            formula, storage, errors,
        ))
    })?;

    let formula =
        errors.block_exec(|errors| Ok(type_inferencer::type_check(formula, storage, errors)))?;

    emitter.emit(&formula.inner.into(), storage)?;
    Ok(())
}
