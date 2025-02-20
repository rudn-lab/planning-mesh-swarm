use alloc::{string::String, vec::Vec};

use crate::{
    action::Action, evaluation::ResolutionContext, object::ObjectCollection,
    r#type::TypeCollection, state::State,
};

pub struct Domain {
    name: String,
    types: TypeCollection,
    // predicates: Vec<PredDeclaration>,
    actions: Vec<Action>,
}

pub struct Problem {
    name: String,
    domain: Domain,
    objects: ObjectCollection,
    init: State,
}

pub struct DefaultSolver;

pub trait Solver {
    fn solve(&self, problem: Problem) -> Vec<Action>;
}

impl Solver for DefaultSolver {
    fn solve(&self, problem: Problem) -> Vec<Action> {
        let state = problem.init;

        for action in problem.domain.actions {
            let _resolution = state.resolve_action(action, &problem.domain.types, &problem.objects);
        }

        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{object::ObjectCollection, r#type::TypeCollection};

    #[test]
    fn test_usage() {
        let types = TypeCollection::default();
        let objects = ObjectCollection::default();

        let domain = Domain {
            name: "sample".to_string(),
            types,
            // predicates: vec![],
            actions: vec![],
        };

        let problem = Problem {
            name: "sample-problem".to_string(),
            domain,
            objects,
            init: State::default(),
        };

        // let solver = DefaultSolver;
        // let _steps = solver.solve(problem);
    }
}
