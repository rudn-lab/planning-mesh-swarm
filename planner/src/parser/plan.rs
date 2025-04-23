use core::ops::Deref;

use alloc::vec::Vec;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, multispace0, multispace1},
    combinator::{map, recognize},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, preceded},
    Compare, IResult, InputLength, InputTake, Parser,
};
use pddl::parsers::{ParseError, Span};

use crate::{action::GroundAction, entity::ObjectStorage, parser::ConstructionError, *};

#[derive(Debug)]
pub enum PlanParseError<'a> {
    ActionDoesNotExist(&'a str),
    ObjectDoesNotExist(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Name<'a>(&'a str);

impl<'a> Deref for Name<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Action<'a>(Name<'a>, Vec<Name<'a>>);

impl<'a> Deref for Action<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Plan<'a>(Vec<Action<'a>>);

fn brackets<F, I, O, E>(f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: FnMut(I) -> IResult<I, O, E>,
    I: nom::InputTakeAtPosition + InputLength + InputTake + Clone + for<'a> Compare<&'a str>,
    <I as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone,
    E: nom::error::ParseError<I>,
{
    delimited(pair(tag("("), multispace0), f, pair(multispace0, tag(")")))
}

fn name(input: Span<'_>) -> IResult<Span<'_>, Name<'_>, ParseError<'_>> {
    map(
        recognize(pair(
            alpha1,
            many0(alt((alphanumeric1, tag("-"), tag("_")))),
        )),
        |name: Span<'_>| Name(*name),
    )
    .parse(input)
}

fn action(input: Span<'_>) -> IResult<Span<'_>, Action<'_>, ParseError<'_>> {
    map(
        brackets(separated_list1(multispace1, name)),
        |mut parts: Vec<Name<'_>>| {
            // Can unwrap because parser parses at least one thing
            let name = parts.remove(0);
            Action(name, parts)
        },
    )(input)
}

fn plan(input: Span<'_>) -> IResult<Span<'_>, Plan, ParseError<'_>> {
    map(
        delimited(
            multispace0,
            // Not using `separated_list0(multispace0, action)`
            // because of https://github.com/rust-bakery/nom/issues/1691
            // and we have to use the same version of nom (7.1.3) as pddl
            brackets(many0(preceded(multispace0, action))),
            multispace0,
        ),
        Plan,
    )(input)
}

pub fn parse_plan<'a>(
    definition: &'a str,
    problem: &problem::Problem,
) -> Result<solver::Plan, ConstructionError<'a>> {
    plan(definition.into())
        .map_err(ConstructionError::SyntaxError)
        .map(|(_, plan)| {
            plan.0
                .into_iter()
                .map(|action| {
                    problem
                        .actions
                        .contains(&action)
                        .then(move || {
                            action
                                .1
                                .clone()
                                .into_iter()
                                .map(|arg| {
                                    problem.entities.get_object(&arg).ok_or({
                                        ConstructionError::PlanBuildError(
                                            PlanParseError::ObjectDoesNotExist(arg.0),
                                        )
                                    })
                                })
                                .collect::<Result<Vec<_>, _>>()
                                .map(|v| GroundAction::new(&action.0, v))
                        })
                        .ok_or({
                            ConstructionError::PlanBuildError(PlanParseError::ActionDoesNotExist(
                                action.0 .0,
                            ))
                        })
                        // Remove nested Result
                        .and_then(|v| v)
                })
                .collect::<Result<Vec<_>, _>>()
                .map(|steps| solver::Plan { steps })
        })
        // Remove nested Result
        .and_then(|v| v)
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use crate::solver::{a_star::AStar, Solver};

    use super::{super::*, *};

    #[test]
    fn test_parse_name() {
        let valid_names = [
            "move",
            "pick-up",
            "room1",
            "robot_arm",
            "box-123",
            "a",
            "task-42b",
        ];

        for &input in &valid_names {
            let maybe_name = name(input.into());
            assert!(
                maybe_name.is_ok(),
                "Expected valid name to parse: '{}'",
                input
            );
            let (_, parsed_name) = maybe_name.unwrap();
            assert_eq!(parsed_name, Name(input));
        }

        let invalid_names = [
            "?x",       // variable, not grounded
            "-move",    // starts with non-letter
            "123robot", // starts with digit
            "move!",    // invalid character
            "pick up",  // space inside
            "",         // empty string
            "(move)",   // parentheses are not part of names
            " move",    // leading space
            "move ",    // trailing space
        ];

        for &input in &invalid_names {
            let maybe_name = name(input.into());
            if let Ok((rem, _)) = maybe_name {
                assert!(
                    rem.len() > 0,
                    "Expected to not parse the whole input: '{}'",
                    input
                );
            } else {
                assert!(
                    maybe_name.is_err(),
                    "Expected invalid name to fail parsing: '{}'",
                    input
                );
            }
        }
    }

    #[test]
    fn test_parse_action() {
        let input = "(move r1 roomA roomB)";
        let (rem, act) = action(input.into()).unwrap();
        assert_eq!(rem.trim().len(), 0);
        assert_eq!(
            act,
            Action(Name("move"), vec![Name("r1"), Name("roomA"), Name("roomB")])
        );

        let input = "(  \n   pick-up \n r1    box1 \n roomB   )";
        let (rem, act) = action(input.into()).unwrap();
        assert_eq!(rem.trim().len(), 0);
        assert_eq!(
            act,
            Action(
                Name("pick-up"),
                vec![Name("r1"), Name("box1"), Name("roomB")]
            )
        );
    }

    #[test]
    fn test_parse_plan() {
        let input = r#"((move r1 roomA roomB))"#;
        let (rem, pl) = plan(input.into()).unwrap();
        assert_eq!(rem.trim().len(), 0);
        assert_eq!(
            pl,
            Plan(vec![Action(
                Name("move"),
                vec![Name("r1"), Name("roomA"), Name("roomB")]
            ),])
        );

        let input = "( (move r1 roomA roomB) (pick-up r1 box1 roomB))";
        let (rem, pl) = plan(input.into()).unwrap();
        assert_eq!(rem.trim().len(), 0);
        assert_eq!(
            pl,
            Plan(vec![
                Action(Name("move"), vec![Name("r1"), Name("roomA"), Name("roomB")]),
                Action(
                    Name("pick-up"),
                    vec![Name("r1"), Name("box1"), Name("roomB")]
                ),
            ])
        );

        let input = r#"(
  (move r1 roomA roomB)
  (pick-up r1 box1 roomB)
  (move r1 roomB roomA)
  (drop r1 box1 roomA)
)"#;
        let (rem, pl) = plan(input.into()).unwrap();
        assert_eq!(rem.trim().len(), 0);
        assert_eq!(
            pl,
            Plan(vec![
                Action(Name("move"), vec![Name("r1"), Name("roomA"), Name("roomB")]),
                Action(
                    Name("pick-up"),
                    vec![Name("r1"), Name("box1"), Name("roomB")]
                ),
                Action(Name("move"), vec![Name("r1"), Name("roomB"), Name("roomA")]),
                Action(Name("drop"), vec![Name("r1"), Name("box1"), Name("roomA")]),
            ])
        );
    }

    #[test]
    fn test_pub_parse_function() {
        let domain = r#"
(define (domain robot-domain)
  (:requirements :strips :typing)
  (:types robot room box)

  (:predicates
    (at ?r - robot ?loc - room)
    (connected ?from - room ?to - room)
    (carrying ?r - robot ?o - box)
    (box-at ?o - box ?loc - room)
  )

  (:action move
    :parameters (?r - robot ?from - room ?to - room)
    :precondition (and (at ?r ?from) (connected ?from ?to))
    :effect (and (not (at ?r ?from)) (at ?r ?to))
  )

  (:action pick-up
    :parameters (?r - robot ?o - box ?loc - room)
    :precondition (and (at ?r ?loc) (box-at ?o ?loc))
    :effect (and (not (box-at ?o ?loc)) (carrying ?r ?o))
  )

  (:action drop
    :parameters (?r - robot ?o - box ?loc - room)
    :precondition (and (at ?r ?loc) (carrying ?r ?o))
    :effect (and (not (carrying ?r ?o)) (box-at ?o ?loc))
  )
)
        "#;

        let problem = r#"
(define (problem simple)
  (:domain robot-domain)
  (:objects
    r1 - robot
    roomA roomB - room
    box1 - box
  )

  (:init
    (at r1 roomA)
    (connected roomA roomB)
    (connected roomB roomA)
    (box-at box1 roomB)
  )

  (:goal
    (and (box-at box1 roomA))
  )
)
        "#;

        let domain = parse_domain(domain).unwrap();
        let problem = parse_problem(problem, &domain).unwrap();

        let astar = AStar;

        let maybe_plan = astar.solve(&problem);

        let Ok(Some(plan)) = maybe_plan else {
            panic!("{:?}", maybe_plan);
        };

        let str_plan = format!("{}", plan);

        let Ok(parsed_plan) = parse_plan(&str_plan, &problem) else {
            panic!("Error parsing plan: \n{}", str_plan);
        };

        assert_eq!(plan, parsed_plan);
    }
}
