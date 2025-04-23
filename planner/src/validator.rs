use alloc::collections::BTreeMap;
use gazebo::dupe::Dupe;

use crate::{
    calculus::{predicate::PredicateError, Evaluable},
    problem::Problem,
    solver::Plan,
    state::ParameterGrounding,
};

#[derive(Debug)]
pub enum ValidationError {
    MissingAction,
    GroundingError(PredicateError),
}

pub fn validate_plan(plan: &Plan, problem: &Problem) -> Result<bool, ValidationError> {
    let mut state = problem.init.clone();

    for step in &plan.steps {
        let Some(action) = problem.actions.get_by_symbol(&step.name) else {
            return Err(ValidationError::MissingAction);
        };

        let grounding = ParameterGrounding(
            action
                .parameters()
                .iter()
                .zip(step.parameters.iter())
                .map(|(ap, o)| (ap, o.dupe()))
                .collect::<BTreeMap<_, _>>(),
        );

        let modifications = match action.ground_effect(&grounding, &state) {
            Ok(modifications) => modifications,
            Err(e) => return Err(ValidationError::GroundingError(e)),
        };

        state = state.modify(modifications);
    }

    let is_goal_reached = problem.goal.eval(&state);

    Ok(is_goal_reached)
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use crate::{
        parser::{parse_domain, parse_problem},
        solver::{a_star::AStar, Solver},
    };

    use super::validate_plan;

    const DOMAIN: &str = r#"
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

    #[test]
    fn test_simple_plan_validation() {
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
    (box-at box1 roomA)
  )
)
        "#;

        let domain = parse_domain(DOMAIN).unwrap();
        let problem = parse_problem(problem, &domain).unwrap();

        let solver = AStar;

        let maybe_plan = solver.solve(&problem);
        if let Ok(Some(plan)) = maybe_plan {
            assert_eq!(plan.steps.len(), 4);
            assert!(validate_plan(&plan, &problem).unwrap());
        } else {
            println!("{:?}", maybe_plan);
            panic!("Error.");
        }
    }
}
