use crate::{
    action::{ActionBuilder, ActionEffect, ActionParameter},
    calculus::{
        first_order::{BoundVariable, QuantifiedFormula, QuantifierBuilder},
        predicate::{Predicate, PredicateBuilder, PredicateDefinition, ResolvedPredicate, Value},
    },
    entity::{ObjectStorage, TypeHandle, TypeStorage},
    problem::{
        BadDefinition as BD, BuildError as BE, Domain, DomainBuilder, Problem,
        UnsupportedFeature as UF, SUPPORTED_REQUIREMENTS,
    },
    util::named::NamedStorage,
    INTERNER,
};
use alloc::{collections::BTreeMap, string::ToString, vec, vec::Vec};
use core::ops::Deref;
use gazebo::dupe::Dupe;
use itertools::Itertools;
use nom::Err;
use pddl::{
    self, AtomicFormula, CEffect, ForallCEffect, GoalDefinition, Name, PEffect, Parser,
    PreconditionGoalDefinition, PreferenceGD, PrimitiveType, StructureDef, Term, Type, TypedList,
    TypedNames, Variable,
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

/// Parses predicates in action's precondition and effect,
/// where they can have variables in them
fn parse_predicate(
    pred: &AtomicFormula<Term>,
    params: &BTreeMap<&str, ActionParameter>,
    bound_vars: &BTreeMap<&str, BoundVariable>,
    objects: &dyn ObjectStorage,
    predicates: &NamedStorage<PredicateDefinition>,
) -> Result<Predicate, BE> {
    match pred {
        AtomicFormula::Equality(_) => Err(BE::UnsupportedFeature(UF::Equality)),
        AtomicFormula::Predicate(pred) => predicates
            .get(pred.predicate())
            .ok_or(BE::BadDefinition(BD::UnknownPredicate(
                pred.predicate().to_string(),
            )))
            .and_then(|p| {
                pred.values()
                    .iter()
                    .map(|v| match v {
                        Term::Name(name) => objects
                            .get_object(name)
                            .ok_or(BE::BadDefinition(BD::UnknownObject(name.to_string())))
                            .map(Value::Object),
                        Term::Variable(var) => {
                            params
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
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .and_then(|v| p.values(v).build().map_err(BE::PredicateError))
            }),
    }
}

/// Parses predicates in problem's init,
/// where they have only concrete objects in them
fn parse_resolved_predicate(
    pred: &AtomicFormula<Name>,
    objects: &dyn ObjectStorage,
    predicates: &NamedStorage<PredicateDefinition>,
) -> Result<ResolvedPredicate, BE> {
    match pred {
        AtomicFormula::Equality(_) => Err(BE::UnsupportedFeature(UF::Equality)),
        AtomicFormula::Predicate(pred) => predicates
            .get(pred.predicate())
            .ok_or(BE::BadDefinition(BD::UnknownPredicate(
                pred.predicate().to_string(),
            )))
            .and_then(|p| {
                pred.values()
                    .iter()
                    .map(|o| {
                        objects
                            .get_object(o)
                            .ok_or(BE::BadDefinition(BD::UnknownObject(o.to_string())))
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .and_then(|v| p.resolved_values(v).build().map_err(BE::PredicateError))
            }),
    }
}

fn parse_goal(
    goal: &GoalDefinition,
    action_params: &BTreeMap<&str, ActionParameter>,
    bound_vars: &BTreeMap<&str, BoundVariable>,
    types: &dyn TypeStorage,
    objects: &dyn ObjectStorage,
    predicates: &NamedStorage<PredicateDefinition>,
) -> Result<QuantifiedFormula<Predicate>, BE> {
    use GoalDefinition::*;
    use QuantifiedFormula as F;
    let parse_gd = |g| parse_goal(g, action_params, bound_vars, types, objects, predicates);
    match goal {
        AtomicFormula(pred) => {
            parse_predicate(pred, action_params, bound_vars, objects, predicates).map(F::pred)
        }
        Literal(_) => Err(BE::BadDefinition(BD::LiteralInGoalDefinition)),
        And(goals) => goals
            .iter()
            .map(parse_gd)
            .collect::<Result<Vec<_>, _>>()
            .map(F::and),
        Or(goals) => goals
            .iter()
            .map(parse_gd)
            .collect::<Result<Vec<_>, _>>()
            .map(F::or),
        Not(goal) => parse_gd(goal).map(F::not),
        Imply(goal_ant, goal_con) => {
            parse_gd(goal_ant).and_then(|goal_ant| {
                parse_gd(goal_con)
                    // Transform implication into disjunction at this step
                    .map(|goal_con| F::or(vec![F::not(goal_ant), goal_con]))
            })
        }
        Exists(vars, goal) => parse_arguments(vars, types)
            .and_then(|(param_names, param_types)| {
                QuantifierBuilder::exists(param_types)
                    .expression(|params| {
                        let bound_vars = bound_vars
                            .clone()
                            .into_iter()
                            .chain(param_names.iter().copied().zip(params.to_vec()))
                            .collect::<BTreeMap<&str, BoundVariable>>();
                        parse_goal(goal, action_params, &bound_vars, types, objects, predicates)
                    })
                    .build()
            })
            .map(F::exists),
        ForAll(vars, goal) => parse_arguments(vars, types)
            .and_then(|(param_names, param_types)| {
                QuantifierBuilder::forall(param_types)
                    .expression(|params| {
                        let bound_vars = bound_vars
                            .clone()
                            .into_iter()
                            .chain(param_names.iter().copied().zip(params.to_vec()))
                            .collect::<BTreeMap<&str, BoundVariable>>();
                        parse_goal(goal, action_params, &bound_vars, types, objects, predicates)
                    })
                    .build()
            })
            .map(F::forall),
        FComp(_) => Err(BE::UnsupportedFeature(UF::NumericFluent)),
    }
}

fn parse_precondition_goal_definition(
    goal: &PreconditionGoalDefinition,
    action_params: &BTreeMap<&str, ActionParameter>,
    bound_vars: &BTreeMap<&str, BoundVariable>,
    types: &dyn TypeStorage,
    objects: &dyn ObjectStorage,
    predicates: &NamedStorage<PredicateDefinition>,
) -> Result<QuantifiedFormula<Predicate>, BE> {
    match goal {
        PreconditionGoalDefinition::Preference(PreferenceGD::Goal(goal)) => {
            parse_goal(goal, action_params, bound_vars, types, objects, predicates)
        }
        PreconditionGoalDefinition::Preference(PreferenceGD::Preference(_)) => {
            Err(BE::UnsupportedFeature(UF::Preference))
        }
        PreconditionGoalDefinition::Forall(vars, goals) => parse_arguments(vars, types)
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
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()
                            .map(|g| {
                                if g.len() == 1 {
                                    g.into_iter().next().unwrap()
                                } else {
                                    QuantifiedFormula::And(g)
                                }
                            })
                    })
                    .build()
            })
            .map(QuantifiedFormula::forall),
    }
}

fn parse_effects(
    effect: &CEffect,
    action_params: &BTreeMap<&str, ActionParameter>,
    bound_vars: &BTreeMap<&str, BoundVariable>,
    types: &dyn TypeStorage,
    objects: &dyn ObjectStorage,
    predicates: &NamedStorage<PredicateDefinition>,
) -> Result<ActionEffect, BE> {
    match effect {
        CEffect::Effect(effect) => match effect {
            PEffect::AtomicFormula(pred) => {
                parse_predicate(pred, action_params, bound_vars, objects, predicates)
                    .map(ActionEffect::pred)
            }
            PEffect::NotAtomicFormula(pred) => {
                parse_predicate(pred, action_params, bound_vars, objects, predicates)
                    .map(ActionEffect::npred)
            }
            PEffect::AssignNumericFluent(_, _, _) => Err(BE::UnsupportedFeature(UF::NumericFluent)),
            PEffect::AssignObjectFluent(_, _) => Err(BE::UnsupportedFeature(UF::ObjectFluent)),
        },
        CEffect::Forall(ForallCEffect { variables, effects }) => parse_arguments(variables, types)
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
                                )
                            })
                            .collect::<Result<_, _>>()
                            .map(ActionEffect::And)
                    })
                    .build()
            })
            .map(ActionEffect::ForAll),
        CEffect::When(_) => Err(BE::UnsupportedFeature(UF::Conditional)),
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

    let unsupported_requirements = domain
        .requirements()
        .iter()
        .filter(|r| !SUPPORTED_REQUIREMENTS.contains(r))
        .cloned()
        .collect_vec();

    if !unsupported_requirements.is_empty() {
        return Err(ParseError::BuildError(BE::UnsupportedRequirements(
            unsupported_requirements,
        )));
    }

    DomainBuilder::new_domain(domain.name())
        .types(|types| {
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
        .consts(|types, objects| add_objects(domain.constants(), types, objects))
        .predicate_definitions(|types, predicates| {
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
        .actions(|types, objects, predicates, actions| {
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
                                            )
                                        })
                                        .collect::<Result<Vec<_>, _>>()
                                        .map(QuantifiedFormula::and)
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
                                                    )
                                                })
                                                .collect::<Result<Vec<_>, _>>()
                                                .map(|e| {
                                                    if e.len() == 1 {
                                                        e.into_iter().next().unwrap()
                                                    } else {
                                                        ActionEffect::And(e)
                                                    }
                                                })
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
        .objects(|types, objects| add_objects(problem.objects(), types, objects))
        .init(|objects, predicates, init| {
            problem
                .init()
                .iter()
                .map(|i| match i {
                    pddl::InitElement::Literal(lit) => match lit {
                        pddl::Literal::AtomicFormula(pred) => {
                            parse_resolved_predicate(pred, objects, predicates)
                                .map(|p| init.insert(p))
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
        .goal(|types, objects, predicates| {
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
                    )
                })
                .collect::<Result<Vec<_>, _>>()
                .map(|g| {
                    if g.len() == 1 {
                        g.into_iter().next().unwrap()
                    } else {
                        QuantifiedFormula::And(g)
                    }
                })
        })
        .build()
        .map_err(ParseError::BuildError)
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;
    use crate::{action::ActionEffect as E, calculus::first_order::QuantifiedFormula as F};

    const SIMPLE_DOMAIN: &str = r#"
(define
    (domain construction)
    (:requirements :strips :typing)
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

        assert!(domain.is_ok());
        let domain = domain.unwrap();

        let correct_domain = DomainBuilder::new_domain("construction")
            .types(|types| {
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
            .consts(|types, objects| {
                let site = types.get_type("site").unwrap();
                let _ = objects.get_or_create_object("mainsite", &site);

                Ok(())
            })
            .predicate_definitions(|types, predicates| {
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
            .actions(|types, _object, predicates, actions| {
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
            .objects(|types, objects| {
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
            .init(|objects, predicates, init| {
                let s1 = objects.get_object("s1").unwrap();
                let b = objects.get_object("b").unwrap();
                let w = objects.get_object("w").unwrap();
                let c = objects.get_object("c").unwrap();

                let on_site = predicates.get("on-site").unwrap();

                init.insert(
                    on_site
                        .resolved_values(vec![b.dupe(), s1.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    on_site
                        .resolved_values(vec![c.dupe(), s1.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    on_site
                        .resolved_values(vec![w.dupe(), s1.dupe()])
                        .build()
                        .unwrap(),
                );

                Ok(())
            })
            .goal(|_types, objects, predicates| {
                let s1 = objects.get_object("s1").unwrap();

                let walls_built = predicates.get("walls-built").unwrap();
                let cables_installed = predicates.get("cables-installed").unwrap();
                let windows_fitted = predicates.get("windows-fitted").unwrap();

                Ok(F::and(vec![
                    F::pred(
                        walls_built
                            .values(vec![Value::object(&s1)])
                            .build()
                            .unwrap(),
                    ),
                    F::pred(
                        cables_installed
                            .values(vec![Value::object(&s1)])
                            .build()
                            .unwrap(),
                    ),
                    F::pred(
                        windows_fitted
                            .values(vec![Value::object(&s1)])
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
  (:requirements :typing :quantified-preconditions)
  (:types
    box location
  )
  (:predicates
    (at ?b - box ?l - location)
    (size-smaller ?b1 - box ?b2 - box)
    (collected ?b - box)
    (different ?b1 - box ?b2 - box)
  )
  (:action move
    :parameters (?b - box ?from - location ?to - location)
    :precondition (and
      (at ?b ?from)
      (forall (?other - box)
        (imply (and (different ?other ?b) (at ?other ?from))
               (size-smaller ?other ?b)
        )
      )
    )
    :effect (and
      (not (at ?b ?from))
      (at ?b ?to)
    )
  )
  (:action collect
    :parameters (?b - box ?l - location)
    :precondition (and
      (at ?b ?l)
      (exists (?other - box)
        (and
          (different ?b ?other)
          (at ?other ?l)
          (size-smaller ?b ?other)
        )
      )
    )
    :effect (collected ?b)
  )
)
"#;

    const MEDIUM_PROBLEM_NO_EQ: &str = r#"
(define (problem move-a-big-box)
  (:domain box-moving)
  (:objects
    box1 box2 box3 - box
    loc1 loc2 - location
  )
  (:init
    (at box1 loc1)
    (at box2 loc1)
    (at box3 loc1)
    (size-smaller box1 box2)
    (size-smaller box1 box3)
    (size-smaller box2 box3)

    ;; different predicates (symmetric)
    (different box1 box2)
    (different box2 box1)
    (different box1 box3)
    (different box3 box1)
    (different box2 box3)
    (different box3 box2)
  )
  (:goal
    (forall (?b - box)
      (imply (at ?b loc2)
             (collected ?b)
      )
    )
  )
)
    "#;

    #[test]
    fn test_medium_problem() {
        let domain = parse_domain(MEDIUM_DOMAIN_NO_EQ);

        assert!(domain.is_ok());
        let domain = domain.unwrap();

        let correct_domain = DomainBuilder::new_domain("box-moving")
            .types(|types| {
                let r#box = types.get_or_create_type("box");
                // NOTE: parser automaticlaly adds object type even if
                // it wasn't defined in the domain
                let object = types.get_or_create_type("object");
                let location = types.get_or_create_type("location");

                let _ = types.create_inheritance(&r#box, &object);
                let _ = types.create_inheritance(&location, &object);

                Ok(())
            })
            .consts(|_, _| Ok(()))
            .predicate_definitions(|types, predicates| {
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
                predicates.insert(
                    PredicateBuilder::new("different").arguments(vec![r#box.dupe(), r#box.dupe()]),
                );

                Ok(())
            })
            .actions(|types, _object, predicates, actions| {
                let r#box = types.get_type("box").unwrap();
                let location = types.get_type("location").unwrap();

                let at = predicates.get("at").unwrap();
                let size_smaller = predicates.get("size-smaller").unwrap();
                let collected = predicates.get("collected").unwrap();
                let different = predicates.get("different").unwrap();

                actions.insert(
                    ActionBuilder::new("move")
                        .parameters(vec![r#box.dupe(), location.dupe(), location.dupe()])
                        .precondition(|params| {
                            let b = &params[0];
                            let from = &params[1];
                            let _to = &params[2];
                            Ok(F::and(vec![
                                F::pred(
                                    at.values(vec![Value::param(b), Value::param(from)])
                                        .build()
                                        .unwrap(),
                                ),
                                F::forall(
                                    QuantifierBuilder::forall(vec![r#box.dupe()])
                                        .expression(|q_params| {
                                            let other = &q_params[0];
                                            Ok(F::or(vec![
                                                F::not(F::and(vec![
                                                    F::pred(
                                                        different
                                                            .values(vec![
                                                                Value::bound(other),
                                                                Value::param(b),
                                                            ])
                                                            .build()
                                                            .unwrap(),
                                                    ),
                                                    F::pred(
                                                        at.values(vec![
                                                            Value::bound(other),
                                                            Value::param(from),
                                                        ])
                                                        .build()
                                                        .unwrap(),
                                                    ),
                                                ])),
                                                F::pred(
                                                    size_smaller
                                                        .values(vec![
                                                            Value::bound(other),
                                                            Value::param(b),
                                                        ])
                                                        .build()
                                                        .unwrap(),
                                                ),
                                            ]))
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
                    ActionBuilder::new("collect")
                        .parameters(vec![r#box.dupe(), location.dupe()])
                        .precondition(|params| {
                            let b = &params[0];
                            let l = &params[1];
                            Ok(F::and(vec![
                                F::pred(
                                    at.values(vec![Value::param(b), Value::param(l)])
                                        .build()
                                        .unwrap(),
                                ),
                                F::exists(
                                    QuantifierBuilder::exists(vec![r#box.dupe()])
                                        .expression(|ex_params| {
                                            let other = &ex_params[0];
                                            Ok(F::and(vec![
                                                F::pred(
                                                    different
                                                        .values(vec![
                                                            Value::param(b),
                                                            Value::bound(other),
                                                        ])
                                                        .build()
                                                        .unwrap(),
                                                ),
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
                                                            Value::param(b),
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
                            ]))
                        })
                        .effect(|params| {
                            let b = &params[0];
                            Ok(E::pred(
                                collected.values(vec![Value::param(b)]).build().unwrap(),
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
            domain.actions.get("move").unwrap(),
            correct_domain.actions.get("move").unwrap()
        );
        assert_eq!(
            domain.actions.get("collect").unwrap(),
            correct_domain.actions.get("collect").unwrap()
        );

        let problem = parse_problem(MEDIUM_PROBLEM_NO_EQ, &domain);

        assert!(problem.is_ok());
        let problem = problem.unwrap();

        let correct_problem = domain
            .new_problem("move-a-big-box")
            .objects(|types, objects| {
                let r#box = types.get_type("box").unwrap();
                let location = types.get_type("location").unwrap();

                let _ = objects.get_or_create_object("box1", &r#box);
                let _ = objects.get_or_create_object("box2", &r#box);
                let _ = objects.get_or_create_object("box3", &r#box);
                let _ = objects.get_or_create_object("loc1", &location);
                let _ = objects.get_or_create_object("loc2", &location);

                Ok(())
            })
            .init(|objects, predicates, init| {
                let box1 = objects.get_object("box1").unwrap();
                let box2 = objects.get_object("box2").unwrap();
                let box3 = objects.get_object("box3").unwrap();
                let loc1 = objects.get_object("loc1").unwrap();

                let at = predicates.get("at").unwrap();
                let size_smaller = predicates.get("size-smaller").unwrap();
                let different = predicates.get("different").unwrap();

                init.insert(
                    at.resolved_values(vec![box1.dupe(), loc1.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    at.resolved_values(vec![box2.dupe(), loc1.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    at.resolved_values(vec![box3.dupe(), loc1.dupe()])
                        .build()
                        .unwrap(),
                );

                init.insert(
                    size_smaller
                        .resolved_values(vec![box1.dupe(), box2.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    size_smaller
                        .resolved_values(vec![box1.dupe(), box3.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    size_smaller
                        .resolved_values(vec![box2.dupe(), box3.dupe()])
                        .build()
                        .unwrap(),
                );

                init.insert(
                    different
                        .resolved_values(vec![box1.dupe(), box2.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    different
                        .resolved_values(vec![box2.dupe(), box1.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    different
                        .resolved_values(vec![box1.dupe(), box3.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    different
                        .resolved_values(vec![box3.dupe(), box1.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    different
                        .resolved_values(vec![box2.dupe(), box3.dupe()])
                        .build()
                        .unwrap(),
                );
                init.insert(
                    different
                        .resolved_values(vec![box3.dupe(), box2.dupe()])
                        .build()
                        .unwrap(),
                );

                Ok(())
            })
            .goal(|types, objects, predicates| {
                let r#box = types.get_type("box").unwrap();

                let loc2 = objects.get_object("loc2").unwrap();

                let at = predicates.get("at").unwrap();
                let collected = predicates.get("collected").unwrap();

                Ok(F::forall(
                    QuantifierBuilder::forall(vec![r#box.dupe()])
                        .expression(|params| {
                            let b = &params[0];
                            Ok(F::or(vec![
                                F::not(F::pred(
                                    at.values(vec![Value::bound(b), Value::object(&loc2)])
                                        .build()
                                        .unwrap(),
                                )),
                                F::pred(collected.values(vec![Value::bound(b)]).build().unwrap()),
                            ]))
                        })
                        .build()
                        .unwrap(),
                ))
            })
            .build()
            .unwrap();

        assert_eq!(problem.name, correct_problem.name);
        assert_eq!(problem.domain_name, correct_problem.domain_name);
        assert_eq!(problem.entities, correct_problem.entities);
        assert_eq!(problem.actions, correct_problem.actions);
        assert_eq!(problem.init, correct_problem.init);
        assert_eq!(problem.goal, correct_problem.goal);
    }
}
