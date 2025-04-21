use crate::{
    action::{ActionBuilder, ActionEffect, ActionParameter},
    calculus::{
        first_order::{BoundVariable, QuantifiedFormula, QuantifierBuilder},
        predicate::{
            GroundPredicate, LiftedPredicate, Predicate, PredicateBuilder, PredicateDefinition,
            PredicateValue, ScopedPredicate, ScopedValue, Value,
        },
    },
    entity::{ObjectStorage, TypeHandle, TypeStorage},
    problem::{
        BadDefinition as BD, BuildError as BE, Domain, DomainBuilder, Problem,
        UnsupportedFeature as UF,
    },
    util::named::NamedStorage,
    INTERNER,
};
use alloc::{
    collections::{BTreeMap, BTreeSet},
    string::ToString,
    vec::Vec,
};
use core::ops::Deref;
use gazebo::dupe::Dupe;
use nom::Err;
use pddl::{
    self, AtomicFormula, CEffect, ConditionalEffect, ForallCEffect, GoalDefinition, Name, PEffect,
    Parser, PreconditionGoalDefinition, PreferenceGD, PrimitiveType, Requirement, StructureDef,
    Term, Type, TypedList, TypedNames, Variable,
};

#[derive(Debug)]
pub enum ParseError<'a> {
    /// The file is written incorrectly.
    SyntaxError(Err<pddl::parsers::ParseError<'a>>),
    /// Error when building a domain or a problem.
    BuildError(BE),
}

fn parse_type(r#type: &Type) -> Result<&PrimitiveType, BE> {
    match r#type {
        Type::Exactly(r#type) => Ok(r#type),
        Type::EitherOf(_) => Err(BE::UnsupportedFeature(UF::EitherOfTypes)),
    }
}

/// Returns a mapping from arg name to type.
/// This is needed because the logic doesn't need the names,
/// but names are used in text definition and therefore are
/// required when constructing stuff after parsing.
fn parse_arguments<'a>(
    variables: &'a TypedList<Variable>,
    types: &'a dyn TypeStorage,
) -> Result<(Vec<&'a str>, Vec<TypeHandle>), BE> {
    variables
        .iter()
        .map(|v| {
            parse_type(v.type_()).and_then(|pt| {
                types
                    .get_type(pt)
                    .ok_or(BE::BadDefinition(BD::UnknownType(pt.to_string())))
                    .map(|t| (v.value().deref().deref(), t))
            })
        })
        .collect()
}

fn parse_predicate<V: PredicateValue, F>(
    pred: &AtomicFormula<Term>,
    predicates: &NamedStorage<PredicateDefinition>,
    parse_term: F,
    requirements: &BTreeSet<Requirement>,
) -> Result<Predicate<V>, BE>
where
    F: Fn(&Term) -> Result<V, BE>,
{
    match pred {
        AtomicFormula::Equality(eq) => {
            if requirements.contains(&Requirement::Equality) {
                parse_term(eq.first()).and_then(|first| {
                    parse_term(eq.second()).map(|second| PredicateBuilder::equality(first, second))
                })
            } else {
                Err(BE::Requires(Requirement::Equality))
            }
        }
        AtomicFormula::Predicate(pred) => predicates
            .get(pred.predicate())
            .ok_or(BE::BadDefinition(BD::UnknownPredicate(
                pred.predicate().to_string(),
            )))
            .and_then(|p| {
                pred.values()
                    .iter()
                    .map(parse_term)
                    .collect::<Result<Vec<_>, _>>()
                    .and_then(|v| p.values(v).build().map_err(BE::PredicateError))
            }),
    }
}

/// Parses predicates in action's precondition and effect,
/// where they can have variables and objects in them
fn parse_full_predicate(
    pred: &AtomicFormula<Term>,
    action_params: &BTreeMap<&str, ActionParameter>,
    bound_vars: &BTreeMap<&str, BoundVariable>,
    objects: &dyn ObjectStorage,
    predicates: &NamedStorage<PredicateDefinition>,
    requirements: &BTreeSet<Requirement>,
) -> Result<LiftedPredicate, BE> {
    let parse_term = |term: &Term| match term {
        Term::Name(name) => objects
            .get_object(name)
            .ok_or(BE::BadDefinition(BD::UnknownObject(name.to_string())))
            .map(Value::Object),
        Term::Variable(var) => {
            action_params
                // First, try getting an action parameter
                .get(var.deref().deref())
                .map(|ap| Value::ActionParameter(ap.dupe()))
                // If that fails, try getting a bound variable
                .or_else(|| {
                    bound_vars
                        .get(var.deref().deref())
                        .map(|bv| Value::BoundVariable(bv.dupe()))
                })
                .ok_or(BE::BadDefinition(BD::UnknownParameter(var.to_string())))
        }
        Term::Function(_) => Err(BE::UnsupportedFeature(UF::Function)),
    };

    parse_predicate(pred, predicates, parse_term, requirements)
}

/// Parses predicates in problem's goal,
/// where they can only have objects and bound variables in quantifiers.
fn parse_goal_predicate(
    pred: &AtomicFormula<Term>,
    bound_vars: &BTreeMap<&str, BoundVariable>,
    objects: &dyn ObjectStorage,
    predicates: &NamedStorage<PredicateDefinition>,
    requirements: &BTreeSet<Requirement>,
) -> Result<ScopedPredicate, BE> {
    let parse_term = |term: &Term| match term {
        Term::Name(name) => objects
            .get_object(name)
            .ok_or(BE::BadDefinition(BD::UnknownObject(name.to_string())))
            .map(ScopedValue::Object),
        Term::Variable(var) => bound_vars
            .get(var.deref().deref())
            .map(|bv| ScopedValue::BoundVariable(bv.dupe()))
            .ok_or(BE::BadDefinition(BD::UnknownParameter(var.to_string()))),
        Term::Function(_) => Err(BE::UnsupportedFeature(UF::Function)),
    };

    parse_predicate(pred, predicates, parse_term, requirements)
}

/// Parses predicates in problem's init,
/// where they have only concrete objects in them
fn parse_ground_predicate(
    pred: &AtomicFormula<Name>,
    objects: &dyn ObjectStorage,
    predicates: &NamedStorage<PredicateDefinition>,
) -> Result<GroundPredicate, BE> {
    let get_obj = |name: &Name| {
        objects
            .get_object(name)
            .ok_or(BE::BadDefinition(BD::UnknownObject(name.to_string())))
    };
    match pred {
        AtomicFormula::Equality(eq) => get_obj(eq.first()).and_then(|first| {
            get_obj(eq.second()).map(|second| PredicateBuilder::equality(first, second))
        }),
        AtomicFormula::Predicate(pred) => predicates
            .get(pred.predicate())
            .ok_or(BE::BadDefinition(BD::UnknownPredicate(
                pred.predicate().to_string(),
            )))
            .and_then(|p| {
                pred.values()
                    .iter()
                    .map(get_obj)
                    .collect::<Result<Vec<_>, _>>()
                    .and_then(|v| p.values(v).build().map_err(BE::PredicateError))
            }),
    }
}

#[allow(clippy::too_many_arguments)]
fn parse_goal<V, F>(
    goal: &GoalDefinition,
    action_params: &BTreeMap<&str, ActionParameter>,
    bound_vars: &BTreeMap<&str, BoundVariable>,
    types: &dyn TypeStorage,
    objects: &dyn ObjectStorage,
    predicates: &NamedStorage<PredicateDefinition>,
    parse_pred: &F,
    requirements: &BTreeSet<Requirement>,
) -> Result<QuantifiedFormula<Predicate<V>>, BE>
where
    V: PredicateValue,
    F: Fn(
        &AtomicFormula<Term>,
        &BTreeMap<&str, ActionParameter>,
        &BTreeMap<&str, BoundVariable>,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
    ) -> Result<Predicate<V>, BE>,
{
    use GoalDefinition::*;
    use QuantifiedFormula as F;
    let parse_gd = |g| {
        parse_goal(
            g,
            action_params,
            bound_vars,
            types,
            objects,
            predicates,
            parse_pred,
            requirements,
        )
    };
    match goal {
        AtomicFormula(pred) => {
            parse_pred(pred, action_params, bound_vars, objects, predicates).map(F::pred)
        }
        Literal(_) => Err(BE::BadDefinition(BD::LiteralInGoalDefinition)),
        And(goals) => goals
            .iter()
            .map(parse_gd)
            .collect::<Result<Vec<_>, _>>()
            .map(|g| maybe_and(g, F::And)),
        Or(goals) => {
            if requirements.contains(&Requirement::DisjunctivePreconditions) {
                goals
                    .iter()
                    .map(parse_gd)
                    .collect::<Result<Vec<_>, _>>()
                    .map(F::or)
            } else {
                Err(BE::Requires(Requirement::DisjunctivePreconditions))
            }
        }
        Not(goal) => {
            if requirements.contains(&Requirement::NegativePreconditions) {
                parse_gd(goal).map(F::not)
            } else {
                Err(BE::Requires(Requirement::NegativePreconditions))
            }
        }
        Imply(goal_ant, goal_con) => {
            if requirements.contains(&Requirement::DisjunctivePreconditions) {
                parse_gd(goal_ant).and_then(|goal_ant| {
                    parse_gd(goal_con).map(|goal_con| F::imply(goal_ant, goal_con))
                })
            } else {
                Err(BE::Requires(Requirement::DisjunctivePreconditions))
            }
        }
        Exists(vars, goal) => {
            if requirements.contains(&Requirement::ExistentialPreconditions) {
                parse_arguments(vars, types)
                    .and_then(|(param_names, param_types)| {
                        QuantifierBuilder::exists(param_types)
                            .expression(|params| {
                                let bound_vars = bound_vars
                                    .clone()
                                    .into_iter()
                                    .chain(param_names.iter().copied().zip(params.to_vec()))
                                    .collect::<BTreeMap<&str, BoundVariable>>();
                                parse_goal(
                                    goal,
                                    action_params,
                                    &bound_vars,
                                    types,
                                    objects,
                                    predicates,
                                    parse_pred,
                                    requirements,
                                )
                            })
                            .build()
                    })
                    .map(F::exists)
            } else {
                Err(BE::Requires(Requirement::ExistentialPreconditions))
            }
        }
        ForAll(vars, goal) => {
            if requirements.contains(&Requirement::UniversalPreconditions) {
                parse_arguments(vars, types)
                    .and_then(|(param_names, param_types)| {
                        QuantifierBuilder::forall(param_types)
                            .expression(|params| {
                                let bound_vars = bound_vars
                                    .clone()
                                    .into_iter()
                                    .chain(param_names.iter().copied().zip(params.to_vec()))
                                    .collect::<BTreeMap<&str, BoundVariable>>();
                                parse_goal(
                                    goal,
                                    action_params,
                                    &bound_vars,
                                    types,
                                    objects,
                                    predicates,
                                    parse_pred,
                                    requirements,
                                )
                            })
                            .build()
                    })
                    .map(F::forall)
            } else {
                Err(BE::Requires(Requirement::UniversalPreconditions))
            }
        }
        FComp(_) => Err(BE::UnsupportedFeature(UF::NumericFluent)),
    }
}

fn maybe_and<T, F: Fn(Vec<T>) -> T>(val: Vec<T>, and: F) -> T {
    if val.len() == 1 {
        val.into_iter().next().unwrap()
    } else {
        and(val)
    }
}

#[allow(clippy::too_many_arguments)]
fn parse_precondition_goal_definition<V, F>(
    goal: &PreconditionGoalDefinition,
    action_params: &BTreeMap<&str, ActionParameter>,
    bound_vars: &BTreeMap<&str, BoundVariable>,
    types: &dyn TypeStorage,
    objects: &dyn ObjectStorage,
    predicates: &NamedStorage<PredicateDefinition>,
    parse_pred: &F,
    requirements: &BTreeSet<Requirement>,
) -> Result<QuantifiedFormula<Predicate<V>>, BE>
where
    V: PredicateValue,
    F: Fn(
        &AtomicFormula<Term>,
        &BTreeMap<&str, ActionParameter>,
        &BTreeMap<&str, BoundVariable>,
        &dyn ObjectStorage,
        &NamedStorage<PredicateDefinition>,
    ) -> Result<Predicate<V>, BE>,
{
    match goal {
        PreconditionGoalDefinition::Preference(PreferenceGD::Goal(goal)) => parse_goal(
            goal,
            action_params,
            bound_vars,
            types,
            objects,
            predicates,
            parse_pred,
            requirements,
        ),
        PreconditionGoalDefinition::Preference(PreferenceGD::Preference(_)) => {
            Err(BE::UnsupportedFeature(UF::Preference))
        }
        PreconditionGoalDefinition::Forall(vars, goals) => {
            if requirements.contains(&Requirement::UniversalPreconditions) {
                parse_arguments(vars, types)
                    .and_then(|(param_names, param_types)| {
                        QuantifierBuilder::forall(param_types)
                            .expression(|params| {
                                let bound_vars = bound_vars
                                    .clone()
                                    .into_iter()
                                    .chain(param_names.iter().copied().zip(params.to_vec()))
                                    .collect::<BTreeMap<&str, BoundVariable>>();
                                goals
                                    .iter()
                                    .map(|precondition_goal_definition| {
                                        parse_precondition_goal_definition(
                                            precondition_goal_definition,
                                            action_params,
                                            &bound_vars,
                                            types,
                                            objects,
                                            predicates,
                                            parse_pred,
                                            requirements,
                                        )
                                    })
                                    .collect::<Result<Vec<_>, _>>()
                                    .map(|g| maybe_and(g, QuantifiedFormula::And))
                            })
                            .build()
                    })
                    .map(QuantifiedFormula::forall)
            } else {
                Err(BE::Requires(Requirement::UniversalPreconditions))
            }
        }
    }
}

fn parse_effect(
    effect: &PEffect,
    action_params: &BTreeMap<&str, ActionParameter>,
    bound_vars: &BTreeMap<&str, BoundVariable>,
    objects: &dyn ObjectStorage,
    predicates: &NamedStorage<PredicateDefinition>,
    requirements: &BTreeSet<Requirement>,
) -> Result<ActionEffect, BE> {
    match effect {
        PEffect::AtomicFormula(pred) => parse_full_predicate(
            pred,
            action_params,
            bound_vars,
            objects,
            predicates,
            requirements,
        )
        .map(ActionEffect::pred),
        PEffect::NotAtomicFormula(pred) => parse_full_predicate(
            pred,
            action_params,
            bound_vars,
            objects,
            predicates,
            requirements,
        )
        .map(ActionEffect::npred),
        PEffect::AssignNumericFluent(_, _, _) => Err(BE::UnsupportedFeature(UF::NumericFluent)),
        PEffect::AssignObjectFluent(_, _) => Err(BE::UnsupportedFeature(UF::ObjectFluent)),
    }
}

fn parse_effects(
    effect: &CEffect,
    action_params: &BTreeMap<&str, ActionParameter>,
    bound_vars: &BTreeMap<&str, BoundVariable>,
    types: &dyn TypeStorage,
    objects: &dyn ObjectStorage,
    predicates: &NamedStorage<PredicateDefinition>,
    requirements: &BTreeSet<Requirement>,
) -> Result<ActionEffect, BE> {
    match effect {
        CEffect::Effect(effect) => parse_effect(
            effect,
            action_params,
            bound_vars,
            objects,
            predicates,
            requirements,
        ),
        CEffect::Forall(ForallCEffect { variables, effects }) => {
            if requirements.contains(&Requirement::UniversalPreconditions) {
                parse_arguments(variables, types)
                    .and_then(|(param_names, param_types)| {
                        QuantifierBuilder::forall(param_types)
                            .expression(|params| {
                                let bound_vars = bound_vars
                                    .clone()
                                    .into_iter()
                                    .chain(param_names.iter().copied().zip(params.to_vec()))
                                    .collect::<BTreeMap<&str, BoundVariable>>();
                                effects
                                    .iter()
                                    .map(|effect| {
                                        parse_effects(
                                            effect,
                                            action_params,
                                            &bound_vars,
                                            types,
                                            objects,
                                            predicates,
                                            requirements,
                                        )
                                    })
                                    .collect::<Result<Vec<_>, _>>()
                                    .map(|e| maybe_and(e, ActionEffect::And))
                            })
                            .build()
                    })
                    .map(ActionEffect::ForAll)
            } else {
                Err(BE::Requires(Requirement::UniversalPreconditions))
            }
        }

        CEffect::When(when) => {
            if requirements.contains(&Requirement::ConditionalEffects) {
                parse_goal(
                    &when.condition,
                    action_params,
                    bound_vars,
                    types,
                    objects,
                    predicates,
                    &|p, ap, bv, o, ps| parse_full_predicate(p, ap, bv, o, ps, requirements),
                    requirements,
                )
                .and_then(|condition| {
                    match &when.effect {
                        ConditionalEffect::Single(effect) => parse_effect(
                            effect,
                            action_params,
                            bound_vars,
                            objects,
                            predicates,
                            requirements,
                        ),
                        ConditionalEffect::All(effects) => effects
                            .iter()
                            .map(|e| {
                                parse_effect(
                                    e,
                                    action_params,
                                    bound_vars,
                                    objects,
                                    predicates,
                                    requirements,
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()
                            .map(|e| maybe_and(e, ActionEffect::And)),
                    }
                    .map(|effect| ActionEffect::when(condition, effect))
                })
            } else {
                Err(BE::Requires(Requirement::ConditionalEffects))
            }
        }
    }
}

fn add_objects(
    to_add: &TypedNames,
    types: &dyn TypeStorage,
    objects: &mut dyn ObjectStorage,
) -> Result<(), BE> {
    to_add
        .iter()
        .map(|c| {
            parse_type(c.type_()).and_then(|pt| {
                types
                    .get_type(pt)
                    .map(|t| {
                        let _ = objects.get_or_create_object(c.value(), &t);
                    })
                    .ok_or(BE::BadDefinition(BD::UnknownType(pt.to_string())))
            })
        })
        .collect::<Result<Vec<()>, _>>()
        .map(|_| ())
}

pub fn parse_domain(definition: &'_ str) -> Result<Domain, ParseError<'_>> {
    let domain = pddl::Domain::from_str(definition).map_err(ParseError::SyntaxError)?;

    if !domain.extends().is_empty() {
        return Err(ParseError::BuildError(BE::UnsupportedFeature(
            UF::ExtendsDomain,
        )));
    }

    DomainBuilder::new_domain(domain.name())
        .requirements(domain.requirements().iter().cloned().collect())
        .types(|_, types| {
            domain
                .types()
                .iter()
                .map(|t| {
                    parse_type(t.type_()).map(|pt| {
                        let sub = types.get_or_create_type(t.value());
                        let sup = types.get_or_create_type(pt);
                        let _ = types.create_inheritance(&sub, &sup);
                    })
                })
                .collect::<Result<Vec<()>, _>>()
                .map(|_| ())
        })
        .consts(|_, types, objects| add_objects(domain.constants(), types, objects))
        .predicate_definitions(|_, types, predicates| {
            domain
                .predicates()
                .iter()
                .map(|p| {
                    parse_arguments(p.variables(), types).map(|(_, param_types)| {
                        predicates
                            .insert(PredicateBuilder::new(p.predicate()).arguments(param_types))
                    })
                })
                .collect::<Result<Vec<()>, _>>()
                .map(|_| ())
        })
        .actions(|requirements, types, objects, predicates, actions| {
            domain
                .structure()
                .iter()
                .map(|s| match s {
                    StructureDef::Action(action) => parse_arguments(action.parameters(), types)
                        .and_then(|(param_names, param_types)| {
                            ActionBuilder::new(action.symbol())
                                .parameters(param_types)
                                .precondition(|params| {
                                    let action_params = param_names
                                        .iter()
                                        .copied()
                                        .zip(params.to_vec())
                                        .collect::<BTreeMap<_, _>>();
                                    action
                                        .precondition()
                                        .iter()
                                        .map(|goal| {
                                            parse_precondition_goal_definition(
                                                goal,
                                                &action_params,
                                                &BTreeMap::new(),
                                                types,
                                                objects,
                                                predicates,
                                                &|p, ap, bv, o, ps| {
                                                    parse_full_predicate(
                                                        p,
                                                        ap,
                                                        bv,
                                                        o,
                                                        ps,
                                                        requirements,
                                                    )
                                                },
                                                requirements,
                                            )
                                        })
                                        .collect::<Result<Vec<_>, _>>()
                                        .map(|p| maybe_and(p, QuantifiedFormula::And))
                                })
                                .effect(|params| {
                                    let params = param_names
                                        .iter()
                                        .copied()
                                        .zip(params.to_vec())
                                        .collect::<BTreeMap<_, _>>();
                                    action
                                        .effect()
                                        .as_ref()
                                        .map(|effects| {
                                            effects
                                                .iter()
                                                .map(|effect| {
                                                    parse_effects(
                                                        effect,
                                                        &params,
                                                        &BTreeMap::new(),
                                                        types,
                                                        objects,
                                                        predicates,
                                                        requirements,
                                                    )
                                                })
                                                .collect::<Result<Vec<_>, _>>()
                                                .map(|e| maybe_and(e, ActionEffect::And))
                                        })
                                        .unwrap_or(Ok(ActionEffect::And(Vec::new())))
                                })
                                .build()
                                .map(|a| actions.insert(a))
                        }),
                    StructureDef::DurativeAction(_) => {
                        Err(BE::UnsupportedFeature(UF::DurativeAction))
                    }
                    StructureDef::Derived(_) => Err(BE::UnsupportedFeature(UF::Derive)),
                })
                .collect::<Result<Vec<_>, _>>()
                .map(|_| ())
        })
        .build()
        .map_err(ParseError::BuildError)
}

pub fn parse_problem<'a>(
    definition: &'a str,
    domain: &'a Domain,
) -> Result<Problem, ParseError<'a>> {
    let problem = pddl::Problem::from_str(definition).map_err(ParseError::SyntaxError)?;

    {
        let interner = INTERNER.lock();
        let domain_name = interner
            .resolve(domain.name)
            .expect("Domain name should be in interner because it was constructed with it.");

        if problem.domain() != domain_name {
            return Err(ParseError::BuildError(BE::BadDefinition(BD::WrongDomain(
                problem.domain().to_string(),
            ))));
        }
    }

    if !problem.constraints().is_empty() {
        return Err(ParseError::BuildError(BE::UnsupportedFeature(
            UF::Constraints,
        )));
    }

    if problem.metric_spec().is_some() {
        return Err(ParseError::BuildError(BE::UnsupportedFeature(
            UF::MetricSpec,
        )));
    }

    if problem.length_spec().is_some() {
        return Err(ParseError::BuildError(BE::UnsupportedFeature(
            UF::LengthSpec,
        )));
    }

    if !problem.requirements().is_empty() {
        return Err(ParseError::BuildError(BE::UnsupportedFeature(
            UF::ProblemRequirements,
        )));
    }

    domain
        .new_problem(problem.name())
        .objects(|_, types, objects| add_objects(problem.objects(), types, objects))
        .init(|_, objects, predicates, init| {
            problem
                .init()
                .iter()
                .map(|i| match i {
                    pddl::InitElement::Literal(lit) => match lit {
                        pddl::Literal::AtomicFormula(pred) => {
                            parse_ground_predicate(pred, objects, predicates).map(|p| {
                                let _ = init.insert(p);
                            })
                        }
                        pddl::Literal::NotAtomicFormula(_) => {
                            Err(BE::UnsupportedFeature(UF::NegationInInit))
                        }
                    },
                    pddl::InitElement::At(_, _) => {
                        Err(BE::UnsupportedFeature(UF::TimedInitLiteral))
                    }
                    pddl::InitElement::IsValue(_, _) => {
                        Err(BE::UnsupportedFeature(UF::NumericFluent))
                    }
                    pddl::InitElement::IsObject(_, _) => {
                        Err(BE::UnsupportedFeature(UF::ObjectFluent))
                    }
                })
                .collect::<Result<Vec<()>, _>>()
                .map(|_| ())
        })
        .goal(|requirements, types, objects, predicates| {
            problem
                .goals()
                .iter()
                .map(|precondition_goal_definition| {
                    parse_precondition_goal_definition(
                        precondition_goal_definition,
                        &BTreeMap::new(),
                        &BTreeMap::new(),
                        types,
                        objects,
                        predicates,
                        &|p, _, bv, o, ps| parse_goal_predicate(p, bv, o, ps, requirements),
                        requirements,
                    )
                })
                .collect::<Result<Vec<_>, _>>()
                .map(|g| maybe_and(g, QuantifiedFormula::And))
        })
        .build()
        .map_err(ParseError::BuildError)
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use pddl::Requirement;

    use super::*;
    use crate::{
        action::ActionEffect as E,
        calculus::{first_order::QuantifiedFormula as F, predicate::ScopedValue},
        problem::BuildError,
    };
    use alloc::collections::BTreeSet;

    const SIMPLE_DOMAIN: &str = r#"
(define
    (domain construction)
    (:requirements :strips :typing :negative-preconditions)
    (:types
        site material - object
        bricks cables windows - material
    )
    (:constants mainsite - site)
    (:predicates
        (walls-built ?s - site)
        (windows-fitted ?s - site)
        (foundations-set ?s - site)
        (cables-installed ?s - site)
        (site-built ?s - site)
        (on-site ?m - material ?s - site)
        (material-used ?m - material)
    )
    (:action BUILD-WALL
        :parameters (?s - site ?b - bricks)
        :precondition (and
            (on-site ?b ?s)
            (foundations-set ?s)
            (not (walls-built ?s))
            (not (material-used ?b))
        )
        :effect (and
            (walls-built ?s)
            (material-used ?b)
        )
    )
)
    "#;

    const SIMPLE_PROBLEM: &str = r#"
(define
    (problem buildingahouse)
    (:domain construction)
    (:objects 
        s1 - site 
        b - bricks 
        w - windows 
        c - cables
    )
    (:init
        (on-site b s1)
        (on-site c s1)
        (on-site w s1)
    )
    (:goal
        (and
            (walls-built s1)
            (cables-installed s1)
            (windows-fitted s1)
        )
    )
)
        "#;

    #[test]
    fn test_simple_problem() {
        let domain = parse_domain(SIMPLE_DOMAIN);

        println!("{:?}", domain);
        assert!(domain.is_ok());
        let domain = domain.unwrap();

        let correct_domain = DomainBuilder::new_domain("construction")
            .requirements(BTreeSet::from([
                Requirement::Strips,
                Requirement::Typing,
                Requirement::NegativePreconditions,
            ]))
            .types(|_, types| {
                let site = types.get_or_create_type("site");
                let object = types.get_or_create_type("object");
                let material = types.get_or_create_type("material");
                let bricks = types.get_or_create_type("bricks");
                let cables = types.get_or_create_type("cables");
                let windows = types.get_or_create_type("windows");

                types.create_inheritance(&site, &object).unwrap();
                types.create_inheritance(&material, &object).unwrap();
                types.create_inheritance(&bricks, &material).unwrap();
                types.create_inheritance(&cables, &material).unwrap();
                types.create_inheritance(&windows, &material).unwrap();

                Ok(())
            })
            .consts(|_, types, objects| {
                let site = types.get_type("site").unwrap();
                let _ = objects.get_or_create_object("mainsite", &site);

                Ok(())
            })
            .predicate_definitions(|_, types, predicates| {
                let site = types.get_type("site").unwrap();
                let material = types.get_type("material").unwrap();

                predicates
                    .insert(PredicateBuilder::new("walls-built").arguments(vec![site.dupe()]));
                predicates
                    .insert(PredicateBuilder::new("windows-fitted").arguments(vec![site.dupe()]));
                predicates
                    .insert(PredicateBuilder::new("foundations-set").arguments(vec![site.dupe()]));
                predicates
                    .insert(PredicateBuilder::new("cables-installed").arguments(vec![site.dupe()]));
                predicates.insert(PredicateBuilder::new("site-built").arguments(vec![site.dupe()]));
                predicates.insert(
                    PredicateBuilder::new("on-site").arguments(vec![material.dupe(), site.dupe()]),
                );
                predicates.insert(
                    PredicateBuilder::new("material-used").arguments(vec![material.dupe()]),
                );
                Ok(())
            })
            .actions(|_, types, _object, predicates, actions| {
                let site = types.get_type("site").unwrap();
                let bricks = types.get_type("bricks").unwrap();

                let on_site = predicates.get("on-site").unwrap();
                let foundations_built = predicates.get("foundations-set").unwrap();
                let walls_built = predicates.get("walls-built").unwrap();
                let material_used = predicates.get("material-used").unwrap();

                let _ = ActionBuilder::new("BUILD-WALL")
                    .parameters(vec![site, bricks])
                    .precondition(|params| {
                        Ok(F::and(vec![
                            F::pred(
                                on_site
                                    .values(vec![
                                        Value::param(&params[1]),
                                        Value::param(&params[0]),
                                    ])
                                    .build()
                                    .unwrap(),
                            ),
                            F::pred(
                                foundations_built
                                    .values(vec![Value::param(&params[0])])
                                    .build()
                                    .unwrap(),
                            ),
                            F::not(F::pred(
                                walls_built
                                    .values(vec![Value::param(&params[0])])
                                    .build()
                                    .unwrap(),
                            )),
                            F::not(F::pred(
                                material_used
                                    .values(vec![Value::param(&params[1])])
                                    .build()
                                    .unwrap(),
                            )),
                        ]))
                    })
                    .effect(|params| {
                        Ok(E::and(vec![
                            E::pred(
                                walls_built
                                    .values(vec![Value::param(&params[0])])
                                    .build()
                                    .unwrap(),
                            ),
                            E::pred(
                                material_used
                                    .values(vec![Value::param(&params[1])])
                                    .build()
                                    .unwrap(),
                            ),
                        ]))
                    })
                    .build()
                    .map(|a| actions.insert(a));
                Ok(())
            })
            .build()
            .unwrap();

        assert_eq!(domain, correct_domain);

        let problem = parse_problem(SIMPLE_PROBLEM, &domain);

        assert!(problem.is_ok());
        let problem = problem.unwrap();

        let correct_problem = domain
            .new_problem("buildingahouse")
            .objects(|_, types, objects| {
                let site = types.get_type("site").unwrap();
                let bricks = types.get_type("bricks").unwrap();
                let windows = types.get_type("windows").unwrap();
                let cables = types.get_type("cables").unwrap();

                let _ = objects.get_or_create_object("s1", &site);
                let _ = objects.get_or_create_object("b", &bricks);
                let _ = objects.get_or_create_object("w", &windows);
                let _ = objects.get_or_create_object("c", &cables);

                Ok(())
            })
            .init(|_, objects, predicates, init| {
                let s1 = objects.get_object("s1").unwrap();
                let b = objects.get_object("b").unwrap();
                let w = objects.get_object("w").unwrap();
                let c = objects.get_object("c").unwrap();

                let on_site = predicates.get("on-site").unwrap();

                init.insert(on_site.values(vec![b.dupe(), s1.dupe()]).build().unwrap());
                init.insert(on_site.values(vec![c.dupe(), s1.dupe()]).build().unwrap());
                init.insert(on_site.values(vec![w.dupe(), s1.dupe()]).build().unwrap());

                Ok(())
            })
            .goal(|_, _types, objects, predicates| {
                let s1 = objects.get_object("s1").unwrap();

                let walls_built = predicates.get("walls-built").unwrap();
                let cables_installed = predicates.get("cables-installed").unwrap();
                let windows_fitted = predicates.get("windows-fitted").unwrap();

                Ok(F::and(vec![
                    F::pred(
                        walls_built
                            .values(vec![ScopedValue::object(&s1)])
                            .build()
                            .unwrap(),
                    ),
                    F::pred(
                        cables_installed
                            .values(vec![ScopedValue::object(&s1)])
                            .build()
                            .unwrap(),
                    ),
                    F::pred(
                        windows_fitted
                            .values(vec![ScopedValue::object(&s1)])
                            .build()
                            .unwrap(),
                    ),
                ]))
            })
            .build()
            .unwrap();

        assert_eq!(problem, correct_problem);
    }

    const MEDIUM_DOMAIN_NO_EQ: &str = r#"
(define (domain box-moving)
  (:requirements :adl)

  (:types
    box location)

  (:predicates
    (at ?b - box ?l - location)
    (size-smaller ?b1 - box ?b2 - box)
    (collected ?b - box)
    (isolated ?l - location))

  (:action move-largest
    :parameters (?b - box ?from - location ?to - location)
    :precondition (and
      (not (= ?from ?to))
      (at ?b ?from)
      (forall (?other - box)
        (imply (and (not (= ?other ?b)) (at ?other ?from))
               (size-smaller ?other ?b))))
    :effect (and
      (not (at ?b ?from))
      (at ?b ?to)))

  (:action collect-largest
    :parameters (?l - location)
    :precondition (forall (?b - box)
      (imply (at ?b ?l)
        (exists (?other - box)
          (and
            (not (= ?b ?other))
            (at ?other ?l)
            (size-smaller ?b ?other)))))
    :effect (forall (?b - box)
      (when (at ?b ?l)
        (collected ?b))))

    (:action isolate-largest
        :parameters (?l - location ?ref - box)
        :precondition (or
          ;; All boxes at location are collected
          (forall (?b - box)
            (imply (at ?b ?l)
                   (collected ?b))
          )
          ;; All boxes at location are smaller than the reference box
          (forall (?b - box)
            (imply (at ?b ?l)
                   (size-smaller ?b ?ref))
          )
        )
        :effect (isolated ?l)))
"#;

    const MEDIUM_PROBLEM_NO_EQ: &str = r#"
(define (problem move-a-big-box)
  (:domain box-moving)
  
  (:objects
    box1 box2 box3 - box
    loc1 loc2 - location)
  
  (:init
    (at box1 loc1)
    (at box2 loc1)
    (at box3 loc1)

    (size-smaller box1 box2)
    (size-smaller box1 box3)
    (size-smaller box2 box3))
  
  (:goal
      (and
        (forall (?b - box)
          (imply (and (at ?b loc2) (not (= ?b box3)))
                 (collected ?b)))
        (isolated loc2))))
"#;

    #[test]
    fn test_medium_problem() {
        let domain = parse_domain(MEDIUM_DOMAIN_NO_EQ);

        assert!(domain.is_ok());
        let domain = domain.unwrap();

        let correct_domain = DomainBuilder::new_domain("box-moving")
            .requirements(BTreeSet::from([Requirement::Adl]))
            .types(|_, types| {
                let r#box = types.get_or_create_type("box");
                // NOTE: parser automaticlaly adds object type even if
                // it wasn't defined in the domain
                let object = types.get_or_create_type("object");
                let location = types.get_or_create_type("location");

                let _ = types.create_inheritance(&r#box, &object);
                let _ = types.create_inheritance(&location, &object);

                Ok(())
            })
            .consts(|_, _, _| Ok(()))
            .predicate_definitions(|_, types, predicates| {
                let r#box = types.get_type("box").unwrap();
                let location = types.get_type("location").unwrap();

                predicates.insert(
                    PredicateBuilder::new("at").arguments(vec![r#box.dupe(), location.dupe()]),
                );
                predicates.insert(
                    PredicateBuilder::new("size-smaller")
                        .arguments(vec![r#box.dupe(), r#box.dupe()]),
                );
                predicates.insert(PredicateBuilder::new("collected").arguments(vec![r#box.dupe()]));
                predicates
                    .insert(PredicateBuilder::new("isolated").arguments(vec![location.dupe()]));

                Ok(())
            })
            .actions(|_, types, _object, predicates, actions| {
                let r#box = types.get_type("box").unwrap();
                let location = types.get_type("location").unwrap();

                let at = predicates.get("at").unwrap();
                let size_smaller = predicates.get("size-smaller").unwrap();
                let collected = predicates.get("collected").unwrap();
                let isolated = predicates.get("isolated").unwrap();

                actions.insert(
                    ActionBuilder::new("move-largest")
                        .parameters(vec![r#box.dupe(), location.dupe(), location.dupe()])
                        .precondition(|params| {
                            let b = &params[0];
                            let from = &params[1];
                            let to = &params[2];
                            Ok(F::and(vec![
                                F::not(F::pred(PredicateBuilder::equality(
                                    Value::param(from),
                                    Value::param(to),
                                ))),
                                F::pred(
                                    at.values(vec![Value::param(b), Value::param(from)])
                                        .build()
                                        .unwrap(),
                                ),
                                F::forall(
                                    QuantifierBuilder::forall(vec![r#box.dupe()])
                                        .expression(|q_params| {
                                            let other = &q_params[0];
                                            Ok(F::imply(
                                                F::and(vec![
                                                    F::not(F::pred(PredicateBuilder::equality(
                                                        Value::bound(other),
                                                        Value::param(b),
                                                    ))),
                                                    F::pred(
                                                        at.values(vec![
                                                            Value::bound(other),
                                                            Value::param(from),
                                                        ])
                                                        .build()
                                                        .unwrap(),
                                                    ),
                                                ]),
                                                F::pred(
                                                    size_smaller
                                                        .values(vec![
                                                            Value::bound(other),
                                                            Value::param(b),
                                                        ])
                                                        .build()
                                                        .unwrap(),
                                                ),
                                            ))
                                        })
                                        .build()
                                        .unwrap(),
                                ),
                            ]))
                        })
                        .effect(|params| {
                            let b = &params[0];
                            let from = &params[1];
                            let to = &params[2];
                            Ok(E::and(vec![
                                E::npred(
                                    at.values(vec![Value::param(b), Value::param(from)])
                                        .build()
                                        .unwrap(),
                                ),
                                E::pred(
                                    at.values(vec![Value::param(b), Value::param(to)])
                                        .build()
                                        .unwrap(),
                                ),
                            ]))
                        })
                        .build()
                        .unwrap(),
                );

                actions.insert(
                    ActionBuilder::new("collect-largest")
                        .parameters(vec![location.dupe()])
                        .precondition(|params| {
                            let l = &params[0];
                            Ok(F::forall(
                                QuantifierBuilder::forall(vec![r#box.dupe()])
                                    .expression(|fa_params| {
                                        let b = &fa_params[0];
                                        Ok(F::imply(
                                            F::pred(
                                                at.values(vec![Value::bound(b), Value::param(l)])
                                                    .build()
                                                    .unwrap(),
                                            ),
                                            F::exists(
                                                QuantifierBuilder::exists(vec![r#box.dupe()])
                                                    .expression(|ex_params| {
                                                        let other = &ex_params[0];
                                                        Ok(F::and(vec![
                                                            F::not(F::pred(
                                                                PredicateBuilder::equality(
                                                                    Value::bound(b),
                                                                    Value::bound(other),
                                                                ),
                                                            )),
                                                            F::pred(
                                                                at.values(vec![
                                                                    Value::bound(other),
                                                                    Value::param(l),
                                                                ])
                                                                .build()
                                                                .unwrap(),
                                                            ),
                                                            F::pred(
                                                                size_smaller
                                                                    .values(vec![
                                                                        Value::bound(b),
                                                                        Value::bound(other),
                                                                    ])
                                                                    .build()
                                                                    .unwrap(),
                                                            ),
                                                        ]))
                                                    })
                                                    .build()
                                                    .unwrap(),
                                            ),
                                        ))
                                    })
                                    .build()
                                    .unwrap(),
                            ))
                        })
                        .effect(|params| {
                            let l = &params[0];
                            Ok(E::forall(
                                QuantifierBuilder::forall(vec![r#box.dupe()])
                                    .expression(|fa_params| {
                                        let b = &fa_params[0];
                                        Ok(E::when(
                                            F::pred(
                                                at.values(vec![Value::bound(b), Value::param(l)])
                                                    .build()
                                                    .unwrap(),
                                            ),
                                            E::pred(
                                                collected
                                                    .values(vec![Value::bound(b)])
                                                    .build()
                                                    .unwrap(),
                                            ),
                                        ))
                                    })
                                    .build()
                                    .unwrap(),
                            ))
                        })
                        .build()
                        .unwrap(),
                );

                actions.insert(
                    ActionBuilder::new("isolate-largest")
                        .parameters(vec![location.dupe(), r#box.dupe()])
                        .precondition(|params| {
                            let l = &params[0];
                            let r#ref = &params[1];
                            Ok(F::or(vec![
                                F::forall(
                                    QuantifierBuilder::forall(vec![r#box.dupe()])
                                        .expression(|fa_params| {
                                            let b = &fa_params[0];
                                            Ok(F::imply(
                                                F::pred(
                                                    at.values(vec![
                                                        Value::bound(b),
                                                        Value::param(l),
                                                    ])
                                                    .build()
                                                    .unwrap(),
                                                ),
                                                F::pred(
                                                    collected
                                                        .values(vec![Value::bound(b)])
                                                        .build()
                                                        .unwrap(),
                                                ),
                                            ))
                                        })
                                        .build()
                                        .unwrap(),
                                ),
                                F::forall(
                                    QuantifierBuilder::forall(vec![r#box.dupe()])
                                        .expression(|fa_params| {
                                            let b = &fa_params[0];
                                            Ok(F::imply(
                                                F::pred(
                                                    at.values(vec![
                                                        Value::bound(b),
                                                        Value::param(l),
                                                    ])
                                                    .build()
                                                    .unwrap(),
                                                ),
                                                F::pred(
                                                    size_smaller
                                                        .values(vec![
                                                            Value::bound(b),
                                                            Value::param(r#ref),
                                                        ])
                                                        .build()
                                                        .unwrap(),
                                                ),
                                            ))
                                        })
                                        .build()
                                        .unwrap(),
                                ),
                            ]))
                        })
                        .effect(|params| {
                            let l = &params[0];
                            Ok(E::pred(
                                isolated.values(vec![Value::param(l)]).build().unwrap(),
                            ))
                        })
                        .build()
                        .unwrap(),
                );

                Ok(())
            })
            .build()
            .unwrap();

        // assert_eq!( domain, correct_domain,);
        assert_eq!(domain.name, correct_domain.name);
        assert_eq!(domain.entities, correct_domain.entities);
        assert_eq!(
            domain.predicate_definitions,
            correct_domain.predicate_definitions
        );
        assert_eq!(
            domain.actions.get("move-largest").unwrap(),
            correct_domain.actions.get("move-largest").unwrap()
        );
        assert_eq!(
            domain.actions.get("collect-largest").unwrap(),
            correct_domain.actions.get("collect-largest").unwrap()
        );
        assert_eq!(
            domain.actions.get("isolate-largest").unwrap(),
            correct_domain.actions.get("isolate-largest").unwrap()
        );

        let problem = parse_problem(MEDIUM_PROBLEM_NO_EQ, &domain);

        assert!(problem.is_ok());
        let problem = problem.unwrap();

        let correct_problem = domain
            .new_problem("move-a-big-box")
            .objects(|_, types, objects| {
                let r#box = types.get_type("box").unwrap();
                let location = types.get_type("location").unwrap();

                let _ = objects.get_or_create_object("box1", &r#box);
                let _ = objects.get_or_create_object("box2", &r#box);
                let _ = objects.get_or_create_object("box3", &r#box);
                let _ = objects.get_or_create_object("loc1", &location);
                let _ = objects.get_or_create_object("loc2", &location);

                Ok(())
            })
            .init(|_, objects, predicates, init| {
                let box1 = objects.get_object("box1").unwrap();
                let box2 = objects.get_object("box2").unwrap();
                let box3 = objects.get_object("box3").unwrap();
                let loc1 = objects.get_object("loc1").unwrap();

                let at = predicates.get("at").unwrap();
                let size_smaller = predicates.get("size-smaller").unwrap();

                init.insert(at.values(vec![box1.dupe(), loc1.dupe()]).build().unwrap());
                init.insert(at.values(vec![box2.dupe(), loc1.dupe()]).build().unwrap());
                init.insert(at.values(vec![box3.dupe(), loc1.dupe()]).build().unwrap());

                init.insert(
                    size_smaller
                        .values(vec![box1.dupe(), box2.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    size_smaller
                        .values(vec![box1.dupe(), box3.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    size_smaller
                        .values(vec![box2.dupe(), box3.dupe()])
                        .build()
                        .unwrap(),
                );

                Ok(())
            })
            .goal(|_, types, objects, predicates| {
                let r#box = types.get_type("box").unwrap();

                let loc2 = &objects.get_object("loc2").unwrap();
                let box3 = &objects.get_object("box3").unwrap();

                let at = predicates.get("at").unwrap();
                let collected = predicates.get("collected").unwrap();
                let isolated = predicates.get("isolated").unwrap();

                Ok(F::and(vec![
                    F::forall(
                        QuantifierBuilder::forall(vec![r#box.dupe()])
                            .expression(|params| {
                                let b = &params[0];
                                Ok(F::imply(
                                    F::and(vec![
                                        F::pred(
                                            at.values(vec![
                                                ScopedValue::bound(b),
                                                ScopedValue::object(loc2),
                                            ])
                                            .build()
                                            .unwrap(),
                                        ),
                                        F::not(F::pred(PredicateBuilder::equality(
                                            ScopedValue::bound(b),
                                            ScopedValue::object(box3),
                                        ))),
                                    ]),
                                    F::pred(
                                        collected
                                            .values(vec![ScopedValue::bound(b)])
                                            .build()
                                            .unwrap(),
                                    ),
                                ))
                            })
                            .build()
                            .unwrap(),
                    ),
                    F::pred(
                        isolated
                            .values(vec![ScopedValue::object(loc2)])
                            .build()
                            .unwrap(),
                    ),
                ]))
            })
            .build()
            .unwrap();

        assert_eq!(problem.name, correct_problem.name);
        assert_eq!(problem.domain_name, correct_problem.domain_name);
        assert_eq!(problem.entities, correct_problem.entities);
        assert_eq!(problem.actions, correct_problem.actions);
        assert_eq!(problem.init, correct_problem.init);
        assert_eq!(problem.init.predicates().count(), 6);
        assert_eq!(problem.goal, correct_problem.goal);
    }

    #[test]
    fn test_basic_strips_typing() {
        let domain = r#"
    (define (domain strips-typing)
      (:requirements :strips :typing)
      (:types robot location)
      (:predicates (at ?r - robot ?l - location))
      (:action move
        :parameters (?r - robot ?from - location ?to - location)
        :precondition (at ?r ?from)
        :effect (and (not (at ?r ?from)) (at ?r ?to))
      )
    )
    "#;

        let result = parse_domain(domain);
        println!("{:?}", result);
        assert!(result.is_ok());
    }

    #[test]
    fn test_mandatory_requirements_missing() {
        let domain = r#"
    (define (domain strips-typing)
      (:requirements :negative-preconditions)
      (:types robot location)
      (:predicates (at ?r - robot ?l - location))
      (:action move
        :parameters (?r - robot ?from - location ?to - location)
        :precondition (at ?r ?from)
        :effect (and (not (at ?r ?from)) (at ?r ?to))
      )
    )
    "#;

        let result = parse_domain(domain);
        if let Err(ParseError::BuildError(BuildError::MissingRequirements(missing))) = result {
            assert_eq!(missing, vec![Requirement::Strips, Requirement::Typing]);
        } else {
            println!("{:?}", result);
            panic!("Not error.")
        }
    }

    #[test]
    fn test_missing_equality_requirement() {
        let domain = r#"
    (define (domain missing-equality)
      (:requirements :strips :typing)
      (:types object)
      (:predicates (eqtest ?x - object ?y - object))
      (:action test-eq
        :parameters (?x - object ?y - object)
        :precondition (= ?x ?y)
        :effect (eqtest ?x ?y)
      )
    )
    "#;

        let result = parse_domain(domain);
        assert!(matches!(
            result,
            Err(ParseError::BuildError(BuildError::Requires(
                Requirement::Equality
            )))
        ));
    }

    #[test]
    fn test_disjunctive_preconditions_ok() {
        let domain = r#"
    (define (domain disj-precond)
      (:requirements :strips :typing :disjunctive-preconditions)
      (:types object)
      (:predicates (p ?x - object) (q ?x - object))
      (:action test
        :parameters (?x - object)
        :precondition (or (p ?x) (q ?x))
        :effect (p ?x)
      )
    )
    "#;

        let result = parse_domain(domain);
        assert!(result.is_ok());
    }

    #[test]
    fn test_disjunctive_preconditions_missing() {
        let domain = r#"
    (define (domain disj-precond-missing)
      (:requirements :strips :typing)
      (:types object)
      (:predicates (p ?x - object) (q ?x - object))
      (:action test
        :parameters (?x - object)
        :precondition (or (p ?x) (q ?x))
        :effect (p ?x)
      )
    )
    "#;

        let result = parse_domain(domain);
        assert!(matches!(
            result,
            Err(ParseError::BuildError(BuildError::Requires(
                Requirement::DisjunctivePreconditions
            )))
        ));
    }

    #[test]
    fn test_quantified_preconditions_ok() {
        let domain = r#"
    (define (domain quantified-precond)
      (:requirements :strips :typing :quantified-preconditions)
      (:types robot location)
      (:predicates
        (at ?r - robot ?l - location)
        (do-stuff ?r - robot)
      )
      (:action test
        :parameters (?r - robot)
        :precondition (forall (?l - location) (at ?r ?l))
        :effect (do-stuff ?r)
      )
    )
    "#;

        let result = parse_domain(domain);
        assert!(result.is_ok());
    }

    #[test]
    fn test_universal_preconditions_missing() {
        let domain = r#"
    (define (domain universal-precond-missing)
      (:requirements :strips :typing)
      (:types robot location)
      (:predicates (at ?r - robot ?l - location))
      (:action test
        :parameters (?r - robot)
        :precondition (forall (?l - location) (at ?r ?l))
        :effect (at ?r ?l)
      )
    )
    "#;

        let result = parse_domain(domain);
        assert!(matches!(
            result,
            Err(ParseError::BuildError(BuildError::Requires(
                Requirement::UniversalPreconditions
            )))
        ));
    }

    #[test]
    fn test_adl_with_all_features() {
        let domain = r#"
    (define (domain adl-full)
      (:requirements :adl)
      (:types object)
      (:predicates (p ?x - object) (q ?x - object))
      (:action test
        :parameters (?x - object)
        :precondition (and (not (p ?x)) (or (p ?x) (= ?x ?x)))
        :effect (when (exists (?y - object) (p ?y)) (q ?x))
      )
    )
    "#;

        let result = parse_domain(domain);
        assert!(result.is_ok());
    }

    #[test]
    fn test_conditional_effects_missing() {
        let domain = r#"
    (define (domain cond-effect-missing)
      (:requirements :strips :typing)
      (:types object)
      (:predicates (p ?x - object) (q ?x - object))
      (:action test
        :parameters (?x - object)
        :effect (when (p ?x) (q ?x))
      )
    )
    "#;

        let result = parse_domain(domain);
        assert!(matches!(
            result,
            Err(ParseError::BuildError(BuildError::Requires(
                Requirement::ConditionalEffects
            )))
        ));
    }

    #[test]
    fn test_conditional_effects_with_adl() {
        let domain = r#"
    (define (domain cond-effect-adl)
      (:requirements :adl)
      (:types object)
      (:predicates (p ?x - object) (q ?x - object))
      (:action test
        :parameters (?x - object)
        :effect (when (p ?x) (q ?x))
      )
    )
    "#;

        let result = parse_domain(domain);
        assert!(result.is_ok());
    }

    #[test]
    fn test_negative_preconditions_missing() {
        let domain = r#"
    (define (domain missing-neg)
      (:requirements :strips :typing)
      (:types object)
      (:predicates (p ?x - object))
      (:action test
        :parameters (?x - object)
        :precondition (not (p ?x))
        :effect (p ?x)
      )
    )
    "#;

        let result = parse_domain(domain);
        assert!(matches!(
            result,
            Err(ParseError::BuildError(BuildError::Requires(
                Requirement::NegativePreconditions
            )))
        ));
    }

    #[test]
    fn test_quantified_expanded() {
        let domain = r#"
    (define (domain quantified-expanded)
      (:requirements :strips :typing :quantified-preconditions)
      (:types object)
      (:predicates (p ?x - object))
      (:action test
        :parameters (?x - object)
        :precondition (and (forall (?y - object) (p ?y)) (exists (?z - object) (p ?z)))
        :effect (p ?x)
      )
    )
    "#;

        let result = parse_domain(domain);
        assert!(result.is_ok());
    }

    #[test]
    fn test_exists_missing_with_only_universal() {
        let domain = r#"
    (define (domain exists-missing)
      (:requirements :strips :typing :universal-preconditions)
      (:types object)
      (:predicates (p ?x - object))
      (:action test
        :parameters (?x - object)
        :precondition (exists (?y - object) (p ?y))
        :effect (p ?x)
      )
    )
    "#;

        let result = parse_domain(domain);
        assert!(matches!(
            result,
            Err(ParseError::BuildError(BuildError::Requires(
                Requirement::ExistentialPreconditions
            )))
        ));
    }

    #[test]
    fn test_quantified_preconditions_with_only_forall() {
        let domain = r#"
    (define (domain only-universal)
      (:requirements :strips :typing :quantified-preconditions)
      (:types object)
      (:predicates (p ?x - object))
      (:action test
        :parameters (?x - object)
        :precondition (forall (?y - object) (p ?y))
        :effect (p ?x)
      )
    )
    "#;

        let result = parse_domain(domain);
        assert!(result.is_ok());
    }

    #[test]
    fn test_unsupported_requirement() {
        let domain = r#"
    (define (domain unsupported-req)
      (:requirements :strips :typing :durative-actions)
      (:types object)
      (:predicates (p ?x - object))
      (:action test
        :parameters (?x - object)
        :precondition (p ?x)
        :effect (p ?x)
      )
    )
    "#;

        let result = parse_domain(domain);

        if let Err(ParseError::BuildError(BuildError::UnsupportedRequirements(reqs))) = result {
            assert_eq!(reqs, vec![Requirement::DurativeActions])
        } else {
            println!("{:?}", result);
            panic!("Not error!")
        }
    }
}
