use std::fmt;

use failure::Error;
use fnv::FnvHashMap;

use gaf::GroundTerm;
use printer::DisplayType;
use socrates_ast::parsed::{ActiveType, TypeRef, TypeSpec};
use socrates_ast::Spanning;
use socrates_errors::ErrorContext;

#[derive(Debug)]
enum Type<'i> {
    Uninterpreted {
        name: &'i str,
        params: Vec<TypeRef>,
    },
    Interpreted {
        name: &'i str,
        super_type: TypeRef,
        instances: Vec<&'i str>,
    },
    Integer {
        name: &'i str,
        min: i64,
        max: i64,
    },
}

#[derive(Debug)]
struct GenericScope<'i> {
    generics: Vec<(Spanning<&'i str>, usize, Option<TypeRef>)>,
}

#[derive(Debug)]
pub struct Predicate<'i> {
    name: &'i str,
    arg_types: Vec<ActiveType>,
    generics: Vec<(&'i str, Option<TypeRef>)>,
}

#[derive(Debug)]
pub enum Function<'i> {
    Uninterpreted {
        name: &'i str,
        return_type: ActiveType,
        arg_types: Vec<ActiveType>,
        generics: Vec<(&'i str, Option<TypeRef>)>,
    },
    Numeric {
        name: &'i str,
        arg_types: Vec<ActiveType>,
        generics: Vec<(&'i str, Option<TypeRef>)>,
    },
}

#[derive(Debug, Default)]
pub struct TypeStorage<'i> {
    types: Vec<Type<'i>>,
    type_name_lookup: FnvHashMap<&'i str, TypeRef>,
    subtypes: FnvHashMap<TypeRef, Vec<TypeRef>>,
    instances: FnvHashMap<&'i str, TypeRef>,

    predicates: FnvHashMap<&'i str, Predicate<'i>>,
    functions: FnvHashMap<&'i str, Function<'i>>,
}

impl<'i> TypeStorage<'i> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_uninterpreted_type(
        &mut self,
        spec: &TypeSpec<'i>,
        errors: &mut ErrorContext<'i>,
    ) -> Result<(), Error> {
        let params = errors.block_exec(|errors| {
            let params = transpose_option(spec.args.as_ref().map(|args| {
                args.iter()
                    .map(|arg| {
                        self.get_type(&arg.inner.name).ok_or_else(|| {
                            errors.push_error_message(
                                arg.pos,
                                format!("type `{}` not found", arg.inner.name.inner),
                            )
                        })
                    })
                    .collect::<Vec<_>>()
                    .into_iter()
                    .collect::<Result<Vec<_>, _>>()
            }));

            if self.get_type(&spec.name).is_some() {
                errors.push_error_message(
                    spec.name.pos,
                    format!("type `{}` is already defined", spec.name.inner),
                );
            }

            params.map_err(|_| format_err!("Type error"))
        })?;

        let index = TypeRef(self.types.len());
        self.types.push(Type::Uninterpreted {
            name: &spec.name.inner,
            params: params.unwrap_or_else(Vec::new),
        });
        self.type_name_lookup.insert(spec.name.inner, index);

        Ok(())
    }

    pub fn add_interpreted_type(
        &mut self,
        name: &Spanning<&'i str>,
        super_type: &TypeSpec<'i>,
        instances: &[Spanning<&'i str>],
        errors: &mut ErrorContext<'i>,
    ) -> Result<(), Error> {
        let (super_type, instances) = errors.block_exec(|errors| {
            let super_type = match self.type_name_lookup.get(&super_type.name.inner) {
                Some(index) => Some(*index),
                None => {
                    errors.push_error_message(
                        super_type.name.pos,
                        format!("type {} not found", super_type.name.inner),
                    );
                    None
                }
            };

            if self.type_name_lookup.get(&name.inner).is_some() {
                errors.push_error_message(name.pos, format!("type {} already defined", name.inner));
            }

            let instances = instances
                .iter()
                .map(|i| {
                    self.instances
                        .get(&i.inner)
                        .map(|_| {
                            errors.push_error_message(
                                i.pos,
                                format!("instance {} already defined", i.inner),
                            );
                            Err(())
                        })
                        .unwrap_or_else(|| Ok(i.inner))
                })
                .collect::<Vec<_>>()
                .into_iter()
                .collect::<Result<Vec<_>, _>>();

            Ok((
                super_type.ok_or_else(|| format_err!("Type error"))?,
                instances.map_err(|_| format_err!("Type error"))?,
            ))
        })?;

        let index = TypeRef(self.types.len());

        self.type_name_lookup.insert(name.inner, index);
        for instance in &instances {
            self.instances.insert(instance, index);
        }
        self.subtypes
            .entry(super_type)
            .or_insert_with(Vec::new)
            .push(index);

        self.types.push(Type::Interpreted {
            name: name.inner,
            super_type,
            instances,
        });

        Ok(())
    }

    pub fn add_integer_type(
        &mut self,
        name: &Spanning<&'i str>,
        min: &Spanning<i64>,
        max: &Spanning<i64>,
        errors: &mut ErrorContext<'i>,
    ) -> Result<(), Error> {
        errors.block_exec(|errors| {
            if self.type_name_lookup.get(&name.inner).is_some() {
                errors.push_error_message(name.pos, format!("type {} already defined", name.inner));
            }

            if min.inner > max.inner {
                errors.push_error_message(
                    (min.pos.0, max.pos.1),
                    "minimum value is larger than maximum value".to_owned(),
                );
            }

            Ok(())
        })?;

        let index = TypeRef(self.types.len());
        self.type_name_lookup.insert(name.inner, index);
        self.types.push(Type::Integer {
            name: name.inner,
            min: min.inner,
            max: max.inner,
        });

        Ok(())
    }

    pub fn add_predicate(
        &mut self,
        name: &Spanning<&'i str>,
        args: &Spanning<Vec<Spanning<TypeSpec<'i>>>>,
        errors: &mut ErrorContext<'i>,
    ) -> Result<(), Error> {
        let mut scope = GenericScope::new();
        let arg_types = errors.block_exec(|errors| {
            if self.predicates.get(name.inner).is_some() {
                errors.push_error_message(
                    name.pos,
                    format!("predicate {} already defined", name.inner),
                );
            }

            let arg_types = args
                .inner
                .iter()
                .map(|arg| self.make_active(&mut scope, &arg.inner, None, errors))
                .collect::<Vec<_>>()
                .into_iter()
                .collect::<Result<Vec<_>, _>>()
                .map_err(|_| format_err!("Type error"))?;

            Ok(arg_types)
        })?;
        let generics = scope
            .finish(errors)
            .map_err(|_| format_err!("Type error"))?;

        self.predicates.insert(
            name.inner,
            Predicate {
                name: name.inner,
                arg_types,
                generics,
            },
        );

        Ok(())
    }

    pub fn add_uninterpreted_function(
        &mut self,
        name: &Spanning<&'i str>,
        return_type: &TypeSpec<'i>,
        args: &Spanning<Vec<Spanning<TypeSpec<'i>>>>,
        errors: &mut ErrorContext<'i>,
    ) -> Result<(), Error> {
        let mut scope = GenericScope::new();
        let (arg_types, return_type) = errors.block_exec(|errors| {
            if self.functions.get(name.inner).is_some() {
                errors.push_error_message(
                    name.pos,
                    format!("function {} already defined", name.inner),
                );
            }

            let arg_types = args
                .inner
                .iter()
                .map(|arg| self.make_active(&mut scope, &arg.inner, None, errors))
                .collect::<Vec<_>>()
                .into_iter()
                .collect::<Result<Vec<_>, _>>()
                .map_err(|_| format_err!("Type error"))?;
            let return_type = self
                .make_active(&mut scope, return_type, None, errors)
                .map_err(|_| format_err!("Type error"))?;

            Ok((arg_types, return_type))
        })?;
        let generics = scope
            .finish(errors)
            .map_err(|_| format_err!("Type error"))?;

        self.functions.insert(
            name.inner,
            Function::Uninterpreted {
                name: name.inner,
                return_type,
                arg_types,
                generics,
            },
        );

        Ok(())
    }

    pub fn add_numeric_function(
        &mut self,
        name: &Spanning<&'i str>,
        args: &Spanning<Vec<Spanning<TypeSpec<'i>>>>,
        errors: &mut ErrorContext<'i>,
    ) -> Result<(), Error> {
        let mut scope = GenericScope::new();

        let arg_types = errors.block_exec(|errors| {
            if self.functions.get(name.inner).is_some() {
                errors.push_error_message(
                    name.pos,
                    format!("function {} already defined", name.inner),
                );
            }

            let arg_types = args
                .inner
                .iter()
                .map(|arg| self.make_active(&mut scope, &arg.inner, None, errors))
                .collect::<Vec<_>>()
                .into_iter()
                .collect::<Result<Vec<_>, _>>()
                .map_err(|_| format_err!("Type error"))?;

            Ok(arg_types)
        })?;
        let generics = scope
            .finish(errors)
            .map_err(|_| format_err!("Type error"))?;

        self.functions.insert(
            name.inner,
            Function::Numeric {
                name: name.inner,
                arg_types,
                generics,
            },
        );

        Ok(())
    }

    fn make_active(
        &self,
        scope: &mut GenericScope<'i>,
        spec: &TypeSpec<'i>,
        parent: Option<TypeRef>,
        errors: &mut ErrorContext<'i>,
    ) -> Result<ActiveType, ()> {
        if let Some(type_ref) = self.get_type(spec.name.inner) {
            let args = transpose_option(spec.args.as_ref().map(|args| {
                let type_params = match &self.types[type_ref.0] {
                    Type::Uninterpreted { params, .. } => params,
                    _ => {
                        errors.push_error_message(
                            args.pos,
                            format!(
                                "type {} does not have any generic parameters",
                                spec.name.inner
                            ),
                        );
                        return Err(());
                    }
                };
                if type_params.len() != args.len() {
                    errors.push_error_message(
                        args.pos,
                        format!(
                            "type {} expects {} generic parameter{}",
                            spec.name.inner,
                            type_params.len(),
                            if type_params.len() == 1 { "" } else { "s" }
                        ),
                    );
                    return Err(());
                }
                args.iter()
                    .zip(type_params)
                    .map(|(arg, param)| self.make_active(scope, arg, Some(*param), errors))
                    .collect::<Vec<_>>()
                    .into_iter()
                    .collect::<Result<Vec<_>, _>>()
            }))?;

            Ok(ActiveType::Ref {
                to: type_ref,
                params: args.unwrap_or_else(Vec::new),
            })
        } else {
            Ok(scope.get_generic_param(spec.name, parent))
        }
    }

    pub fn get_type(&self, name: &str) -> Option<TypeRef> {
        self.type_name_lookup.get(name).cloned()
    }

    pub fn get_instance(&self, name: &str) -> Option<TypeRef> {
        self.instances.get(name).cloned()
    }

    pub fn get_predicate(&self, name: &str) -> Option<&Predicate<'i>> {
        self.predicates.get(name)
    }

    pub fn get_function(&self, name: &str) -> Option<&Function<'i>> {
        self.functions.get(name)
    }

    pub fn type_name(&self, type_ref: TypeRef) -> &str {
        match &self.types[type_ref.0] {
            Type::Integer { name, .. }
            | Type::Interpreted { name, .. }
            | Type::Uninterpreted { name, .. } => name,
        }
    }

    pub fn type_params(&self, type_ref: TypeRef) -> &[TypeRef] {
        match &self.types[type_ref.0] {
            Type::Integer { .. } | Type::Interpreted { .. } => &[],
            Type::Uninterpreted { params, .. } => params,
        }
    }

    pub fn is_integer_type(&self, type_ref: TypeRef) -> bool {
        matches!(&self.types[type_ref.0], Type::Integer { .. })
    }

    pub fn is_interpreted_type(&self, type_ref: TypeRef) -> bool {
        match &self.types[type_ref.0] {
            Type::Interpreted { .. } => true,
            Type::Uninterpreted { name: _, params: _ } => todo!(),
            Type::Integer {
                name: _,
                min: _,
                max: _,
            } => todo!(),
        }
    }

    pub fn is_uninterpreted_type(&self, type_ref: TypeRef) -> bool {
        matches!(&self.types[type_ref.0], Type::Uninterpreted { .. })
    }

    pub fn is_in_hierarchy(&self, subtype: TypeRef, supertype: TypeRef) -> bool {
        if subtype == supertype {
            return true;
        }

        match (&self.types[subtype.0], &self.types[supertype.0]) {
            (Type::Interpreted { super_type, .. }, Type::Uninterpreted { .. }) => {
                self.is_in_hierarchy(*super_type, supertype)
            }
            _ => false,
        }
    }

    pub fn instance_iter(&self, type_ref: TypeRef) -> Vec<GroundTerm<'i>> {
        let subinstances = self
            .subtypes
            .get(&type_ref)
            .map(|v| v.as_slice())
            .unwrap_or_else(|| &[])
            .iter()
            .flat_map(|tr| self.instance_iter(*tr));

        match &self.types[type_ref.0] {
            Type::Interpreted { instances, .. } => subinstances
                .chain(instances.iter().map(|n| GroundTerm::new_ident(n)))
                .collect(),
            Type::Integer { min, max, .. } => subinstances
                .chain((*min..=*max).map(GroundTerm::Number))
                .collect(),
            _ => subinstances.collect(),
        }
    }
}

fn transpose_option<T, E>(r: Option<Result<T, E>>) -> Result<Option<T>, E> {
    match r {
        Some(Ok(v)) => Ok(Some(v)),
        Some(Err(e)) => Err(e),
        None => Ok(None),
    }
}

impl<'i> GenericScope<'i> {
    fn new() -> Self {
        GenericScope {
            generics: Vec::new(),
        }
    }

    fn get_generic_param(
        &mut self,
        name: Spanning<&'i str>,
        upper_bound: Option<TypeRef>,
    ) -> ActiveType {
        for (index, (existing_name, usages, current_upper_bound)) in
            self.generics.iter_mut().enumerate()
        {
            if existing_name.inner == name.inner {
                *usages += 1;
                if current_upper_bound.is_none() && upper_bound.is_some() {
                    // FIXME find lowest upper bound
                    *current_upper_bound = upper_bound;
                }
                return ActiveType::GenericParam { index };
            }
        }

        let index = self.generics.len();
        self.generics.push((name, 1, upper_bound));
        ActiveType::GenericParam { index }
    }

    fn finish(self, errors: &mut ErrorContext<'i>) -> Result<Vec<(&'i str, Option<TypeRef>)>, ()> {
        self.generics
            .into_iter()
            .map(|(name, usages, upper_bound)|
                if usages == 1 {
                    errors.push_error_message(name.pos, format!("type `{}` not found. If this is a generic parameter, it needs to be used more than once", name.inner));
                    Err(())
                }
                else {
                    Ok((name.inner, upper_bound))
                })
            .collect()
    }
}

impl<'i> Function<'i> {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Function::Numeric { .. })
    }

    pub fn is_uninterpreted(&self) -> bool {
        matches!(self, Function::Uninterpreted { .. })
    }

    pub fn arg_types(&self) -> &[ActiveType] {
        match self {
            Function::Numeric { arg_types, .. } | Function::Uninterpreted { arg_types, .. } => {
                arg_types
            }
        }
    }

    pub fn generics(&self) -> &[(&'i str, Option<TypeRef>)] {
        match self {
            Function::Numeric { generics, .. } | Function::Uninterpreted { generics, .. } => {
                generics
            }
        }
    }

    pub fn return_type(&self) -> Option<&ActiveType> {
        match self {
            Function::Numeric { .. } => None,
            Function::Uninterpreted { return_type, .. } => Some(return_type),
        }
    }
}

impl<'i> Predicate<'i> {
    pub fn arg_types(&self) -> &[ActiveType] {
        &self.arg_types
    }

    pub fn generics(&self) -> &[(&'i str, Option<TypeRef>)] {
        &self.generics
    }
}

impl<'i> fmt::Display for DisplayType<'i, ActiveType> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.ty {
            ActiveType::GenericParam { index } => write!(f, "${}", index),
            ActiveType::Ref { to, params } => {
                if params.is_empty() {
                    write!(f, "{}", self.display(to))
                } else {
                    let params = params
                        .iter()
                        .map(|p| format!("{}", self.display(p)))
                        .collect::<Vec<_>>();
                    write!(f, "{}[{}]", self.display(to), params.join(", "))
                }
            }
        }
    }
}
