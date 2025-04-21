use crate::{action::GroundAction, problem::Problem};
use alloc::{collections::BTreeSet, vec::Vec};
use core::{error::Error, fmt::Display};
use pddl::Requirement;

pub mod a_star;

pub trait Solver {
    type Error: Error;
    /// Whether this solver can solve the problem with
    /// these requirements.
    ///
    /// If this returns `true` the solver should correctly
    /// handle all [Domain](crate::problem::Domain) and [Problem] features
    /// that these [Requirement]s enable.
    fn can_solve(&self, requirements: BTreeSet<Requirement>) -> bool;
    /// Solve the given [Problem] and return a [Plan].
    fn solve(&self, problem: Problem) -> Result<Option<Plan>, Self::Error>;
}

/// A sequence of actions that
/// lead from the initial to the goal [State](crate::state::State).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Plan {
    steps: Vec<GroundAction>,
}

impl Display for Plan {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        writeln!(f, "(")?;
        for step in &self.steps {
            writeln!(f, "  {}", step)?;
        }
        write!(f, ")")
    }
}
