use super::*;
use crate::{action::GroundAction, calculus::Evaluable, problem::Problem, state::State};
use alloc::{
    collections::{BTreeMap, BTreeSet, BinaryHeap},
    rc::Rc,
    vec::Vec,
};
use core::{cmp::Ordering, error::Error, fmt::Display};
use pddl::Requirement;

pub struct AStar;

#[derive(Debug)]
pub enum AStarError {}

impl Display for AStarError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for AStarError {}

impl Solver for AStar {
    type Error = AStarError;

    fn can_solve(&self, _: BTreeSet<Requirement>) -> bool {
        true
    }

    fn solve(&self, problem: &Problem) -> Result<Option<Plan>, Self::Error> {
        let mut open = BinaryHeap::new();
        let mut closed: BTreeMap<Rc<State>, usize> = BTreeMap::new();

        open.push(Node {
            state: Rc::new(problem.init.clone()),
            cost: 0,
            estimate: heuristic(&problem.init),
            path: Vec::new(),
        });

        while let Some(Node {
            state,
            cost,
            estimate: _,
            path,
        }) = open.pop()
        {
            closed.insert(Rc::clone(&state), cost);

            if problem.goal.eval(&*state) {
                return Ok(Some(Plan { steps: path }));
            }

            for action in problem.actions.values() {
                for grounding in state.ground_action(action) {
                    let effects = action.ground_effect(&grounding, &*state);
                    if let Ok(effects) = effects {
                        let new_state = Rc::new(state.modify(effects.clone()));

                        if Rc::ptr_eq(&new_state, &state) {
                            // Nothing changed
                            continue;
                        }

                        let new_cost = cost + 1; // uniform cost
                        let new_estimated_total = new_cost + heuristic(&new_state);

                        if let Some(&prev_cost) = closed.get(&new_state) {
                            // Already seen new_state with better cost
                            if new_cost >= prev_cost {
                                continue;
                            }
                        }

                        if let Ok(ground_action) = action.ground(&grounding) {
                            open.push(Node {
                                state: new_state,
                                cost: new_cost,
                                estimate: new_estimated_total,
                                path: {
                                    let mut new_path = path.clone();
                                    new_path.push(ground_action);
                                    new_path
                                },
                            });
                        } else {
                            todo!("Handle possible action grounding error.")
                        }
                    }
                }
            }
        }
        Ok(None)
    }
}

#[derive(Debug, Clone)]
struct Node {
    state: Rc<State>,
    cost: usize,
    estimate: usize,
    path: Vec<GroundAction>,
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.estimate == other.estimate
    }
}

impl Eq for Node {}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(other.estimate.cmp(&self.estimate)) // min-heap
    }
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other.estimate.cmp(&self.estimate)
    }
}

fn heuristic(_state: &State) -> usize {
    // You can improve this with domain-specific knowledge
    0 // for now, acts as uniform-cost search (Dijkstra)
}

#[cfg(test)]
#[coverage(off)]
mod tests {
    use super::*;
    use crate::parser::*;

    #[test]
    fn test_use_min_heap() {
        let a = Node {
            state: Rc::new(State::default()),
            cost: 1,
            estimate: 1,
            path: vec![],
        };
        let b = Node {
            state: Rc::new(State::default()),
            cost: 1,
            estimate: 2,
            path: vec![],
        };

        // a is greater because estimate is lower
        assert_eq!(a.cmp(&b), Ordering::Greater)
    }

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
    fn test_search_trivial() {
        let problem = r#"
(define (problem simple)
  (:domain robot-domain)
  (:objects
    r1 - robot
    roomA - room
  )

  (:init
    (at r1 roomA)
  )

  (:goal
    (at r1 roomA)
  )
)
        "#;

        let domain = parse_domain(DOMAIN).unwrap();
        let problem = parse_problem(problem, &domain).unwrap();

        let solver = AStar;

        let maybe_plan = solver.solve(&problem);
        if let Ok(Some(plan)) = maybe_plan {
            assert_eq!(plan.steps, vec![]);
        } else {
            panic!("{:?}", maybe_plan);
        }
    }

    #[test]
    fn test_search_single_action() {
        let problem = r#"
(define (problem simple)
  (:domain robot-domain)
  (:objects
    r1 - robot
    roomA roomB - room
  )

  (:init
    (at r1 roomA)
    (connected roomA roomB)
    (connected roomB roomA)
  )

  (:goal
    (at r1 roomB)
  )
)
        "#;

        let domain = parse_domain(DOMAIN).unwrap();
        let problem = parse_problem(problem, &domain).unwrap();

        let solver = AStar;

        let maybe_plan = solver.solve(&problem);
        if let Ok(Some(plan)) = maybe_plan {
            assert_eq!(plan.steps.len(), 1);
        } else {
            panic!("{:?}", maybe_plan);
        }
    }

    #[test]
    fn test_search() {
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

        let solution = r#"
(
  (move r1 roomA roomB)
  (pick-up r1 box1 roomB)
  (move r1 roomB roomA)
  (drop r1 box1 roomA)
)
        "#;

        let domain = parse_domain(DOMAIN).unwrap();
        let problem = parse_problem(problem, &domain).unwrap();

        let solver = AStar;

        let maybe_plan = solver.solve(&problem);
        if let Ok(Some(plan)) = maybe_plan {
            let correct_plan = parse_plan(solution, &problem).unwrap();
            assert_eq!(plan, correct_plan);
        } else {
            panic!("{:?}", maybe_plan);
        }
    }
}
