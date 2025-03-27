use crate::{
    action::{ActionBuilder, ActionParameter},
    calculus::{
        predicate::{
            IsPredicate, Predicate, PredicateBuilder, PredicateDefinition, ResolvedPredicate, Value,
        },
        propositional::{And, Formula, FormulaMembers, Primitives},
    },
    entity::{ObjectStorage, TypeHandle, TypeStorage},
    problem::{
        BadDefinition as BD, BuildError as BE, Domain, DomainBuilder, Problem,
        UnsupportedFeature as UF,
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
    self, AtomicFormula, CEffect, GoalDefinition, Name, PEffect, Parser,
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
) -> Result<BTreeMap<&'a str, TypeHandle>, BE> {
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
    params: &[ActionParameter],
    params_map: &BTreeMap<&str, TypeHandle>,
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
                    .zip(p.arguments())
                    .map(|(v, t)| match v {
                        Term::Name(name) => objects
                            .get_object(name, t)
                            .ok_or(BE::BadDefinition(BD::UnknownObject(name.to_string())))
                            .map(Value::Object),
                        Term::Variable(var) => params_map
                            .iter()
                            .find_position(|(k, _)| ***k == ***var)
                            .ok_or(BE::BadDefinition(BD::UnknownParameter(var.to_string())))
                            .and_then(|(idx, _)| {
                                params
                                    .get(idx)
                                    .ok_or(BE::BadDefinition(BD::UnknownParameter(var.to_string())))
                                    .map(|ap| Value::ActionParameter(ap.dupe()))
                            }),
                        Term::Function(_) => Err(BE::UnsupportedFeature(UF::Function)),
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .and_then(|v| p.values(&v).build().map_err(BE::PredicateError))
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
                    .zip(p.arguments())
                    .map(|(o, t)| {
                        objects
                            .get_object(o, t)
                            .ok_or(BE::BadDefinition(BD::UnknownObject(o.to_string())))
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .and_then(|v| p.resolved_values(&v).build().map_err(BE::PredicateError))
            }),
    }
}

/// Parses predicates in problem's goal,
/// where even though they should be resolved they might still use the syntax
/// for variables `?name` instead of just `name`
fn parse_predicate_as_resolved(
    pred: &AtomicFormula<Term>,
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
                    .map(|o| match o {
                        Term::Name(name) => Ok(name.deref()),
                        // WARN: treating variable syntax as object name,
                        // because some PDDL version allow `?name` syntax in goal definition
                        Term::Variable(var) => Ok(var.deref().deref()),
                        Term::Function(_) => Err(BE::UnsupportedFeature(UF::Function)),
                    })
                    .zip(p.arguments())
                    .map(|(maybe_object, t)| {
                        maybe_object.and_then(|o| {
                            objects
                                .get_object(o, t)
                                .ok_or(BE::BadDefinition(BD::UnknownObject(o.to_string())))
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .and_then(|v| p.resolved_values(&v).build().map_err(BE::PredicateError))
            }),
    }
}

fn parse_goal_definition<O, P>(
    goal: &GoalDefinition,
    // params: &'a [ActionParameter],
    // params_map: &'a BTreeMap<&'a str, TypeHandle>,
    // objects: &'a dyn ObjectStorage,
    // predicates: &NamedStorage<PredicateDefinition>,
    op: &O,
) -> Result<FormulaMembers<P>, BE>
where
    O: Fn(&AtomicFormula<pddl::Term>) -> Result<P, BE>,
    P: IsPredicate<P>,
{
    use FormulaMembers as FM;
    use GoalDefinition::*;
    match goal {
        AtomicFormula(pred) => {
            // parse_predicate(pred, params, params_map, objects, predicates).map(FM::pred)
            op(pred).map(FM::pred)
        }
        // Literal contains AtomicFormula inside.
        // Docs say that this is requires Negative Precondition,
        // but tests show that this is not needed.
        Literal(_) => {
            unreachable!("This variant is only used in timed or quantified goal definitions.")
        }
        And(goals) => goals
            .iter()
            .map(|g| parse_goal_definition(g, op))
            .collect::<Result<Vec<_>, _>>()
            .map(FM::and),
        Or(goals) => goals
            .iter()
            .map(|g| parse_goal_definition(g, op))
            .collect::<Result<Vec<_>, _>>()
            .map(FM::or),
        Not(goal) => parse_goal_definition(goal, op).map(FM::not),
        Imply(goal_ant, goal_con) => {
            parse_goal_definition(goal_ant, op).and_then(|goal_ant| {
                parse_goal_definition(goal_con, op)
                    // Transform implication into disjunction at this step
                    .map(|goal_con| FM::or(vec![FM::not(goal_ant), goal_con]))
            })
        }
        Exists(_, _) | ForAll(_, _) => Err(BE::UnsupportedFeature(UF::Quantifier)),
        FComp(_) => Err(BE::UnsupportedFeature(UF::NumericFluent)),
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
        .filter_map(|r| {
            use pddl::Requirement::*;
            match r {
                Strips | Typing | NegativePreconditions | DisjunctivePreconditions => None,
                // // Requires constructing predicates with evaluation strategy
                // Equality => false,
                // // This requires implementing first-order calculus
                // ExistentialPreconditions | UniversalPreconditions | QuantifiedPreconditions => false,
                // // Requires implementing `when`, which will likely happen
                // // during first-order calculus implementation
                // ConditionalEffects => false,
                // // Will not do
                // Fluents | NumericFluents | ObjectFluents => false,
                // // ADL is a super requirement which adds:
                // // strips, typing, disjunctive-preconditions,
                // // equality, quantified-preconditions, conditional-effects.
                // // So we disallow this because conditional effects are not supported
                // Adl => false,
                // // No idea what these are
                // DurativeActions | DurationInequalities | ContinuousEffects | DerivedPredicates
                // | TimedInitialLiterals | Preferences | Constraints | ActionCosts => false,
                r => Some(r),
            }
        })
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
                    parse_arguments(p.variables(), types).map(|params_map| {
                        predicates.insert(
                            PredicateBuilder::new(p.predicate())
                                .arguments(&params_map.into_values().collect_vec()),
                        )
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
                        .map(|params_map| {
                            ActionBuilder::new(action.symbol())
                                .parameters(&params_map.clone().into_values().collect_vec())
                                .precondition(|params| {
                                    action
                                        .precondition()
                                        .iter()
                                        .map(|p| match p {
                                            PreconditionGoalDefinition::Preference(
                                                PreferenceGD::Goal(goal),
                                            ) => parse_goal_definition::<_, Predicate>(
                                                goal,
                                                &|pred| {
                                                    parse_predicate(
                                                        pred,
                                                        params,
                                                        &params_map,
                                                        objects,
                                                        predicates,
                                                    )
                                                },
                                            ),
                                            PreconditionGoalDefinition::Preference(
                                                PreferenceGD::Preference(_),
                                            ) => Err(BE::UnsupportedFeature(UF::Preference)),
                                            PreconditionGoalDefinition::Forall(_, _) => {
                                                Err(BE::UnsupportedFeature(UF::Quantifier))
                                            }
                                        })
                                        .collect::<Result<Vec<_>, _>>()
                                        .map(|and_parts| {
                                            Formula::new(FormulaMembers::and(and_parts))
                                        })
                                })
                                .effect(|params| {
                                    action
                                        .effect()
                                        .as_ref()
                                        .map(|effects| {
                                            effects
                                                .iter()
                                                .map(|effect| match effect {
                                                    CEffect::Effect(effect) => match effect {
                                                        PEffect::AtomicFormula(pred) => {
                                                            parse_predicate(
                                                                pred,
                                                                params,
                                                                &params_map,
                                                                objects,
                                                                predicates,
                                                            )
                                                            .map(Primitives::pred)
                                                        }
                                                        PEffect::NotAtomicFormula(pred) => {
                                                            parse_predicate(
                                                                pred,
                                                                params,
                                                                &params_map,
                                                                objects,
                                                                predicates,
                                                            )
                                                            .map(Primitives::not)
                                                        }
                                                        PEffect::AssignNumericFluent(_, _, _) => {
                                                            Err(BE::UnsupportedFeature(
                                                                UF::NumericFluent,
                                                            ))
                                                        }
                                                        PEffect::AssignObjectFluent(_, _) => {
                                                            Err(BE::UnsupportedFeature(
                                                                UF::ObjectFluent,
                                                            ))
                                                        }
                                                    },
                                                    CEffect::Forall(_) | CEffect::When(_) => {
                                                        Err(BE::UnsupportedFeature(UF::Quantifier))
                                                    }
                                                })
                                                .collect::<Result<Vec<_>, _>>()
                                                .map(And::new)
                                        })
                                        .unwrap_or(Ok(And::new(Vec::new())))
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

    #[cfg(test)]
    {
        println!("{:#?}", problem);
    }

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
        .init(|_types, objects, predicates, init| {
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
                            unreachable!("Negated predicates are not allowed in init.")
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
        .goal(|_types, objects, predicates| {
            problem
                .goals()
                .iter()
                .map(|p| match p {
                    PreconditionGoalDefinition::Preference(PreferenceGD::Goal(goal)) => {
                        parse_goal_definition(goal, &|pred| {
                            parse_predicate_as_resolved(pred, objects, predicates)
                        })
                    }
                    PreconditionGoalDefinition::Preference(PreferenceGD::Preference(_)) => {
                        Err(BE::UnsupportedFeature(UF::Preference))
                    }
                    PreconditionGoalDefinition::Forall(_, _) => {
                        Err(BE::UnsupportedFeature(UF::Quantifier))
                    }
                })
                .collect::<Result<Vec<_>, _>>()
                .map(|and_parts| Formula::new(FormulaMembers::and(and_parts)))
        })
        .build()
        .map_err(ParseError::BuildError)
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;

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
    fn test_simple_domian() {
        let _domain = parse_domain(SIMPLE_DOMAIN).unwrap();
    }

    #[test]
    fn test_problem_parsing() {
        let domain = parse_domain(SIMPLE_DOMAIN).unwrap();
        let _problem = parse_problem(SIMPLE_PROBLEM, &domain).unwrap();
    }
}
