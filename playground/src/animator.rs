use bevy::prelude::*;
use bevy_tweening::{AnimatorState, BoxedTweenable, ComponentTarget, TweenCompleted, Tweenable};

use crate::clock::Simulation;

/// Copy of bevy_tweening::Animator but using Time<Simulation> instead.
/// ---
/// Component to control the animation of another component.
///
/// The animated component is the component located on the same entity as the
/// [`Animator<T>`] itself.
#[derive(Component)]
pub(crate) struct SimAnimator<T: Component> {
    /// Control if this animation is played or not.
    pub(crate) state: AnimatorState,
    pub(crate) tweenable: BoxedTweenable<T>,
    speed: f32,
}

#[derive(Default)]
pub(crate) struct SimAnimatorPlugin<T: Component> {
    _marker: std::marker::PhantomData<T>,
}

impl<T: Component> Plugin for SimAnimatorPlugin<T> {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, component_animator_system::<T>);
    }
}

impl<T: Component> SimAnimator<T> {
    #[must_use]
    pub(crate) fn new(tween: impl Tweenable<T> + 'static) -> Self {
        Self {
            state: default(),
            tweenable: Box::new(tween),
            speed: 1.,
        }
    }

    pub(crate) fn set_tweenable(&mut self, tween: impl Tweenable<T> + 'static) {
        self.tweenable = Box::new(tween);
    }
}

/// Animator system for components.
///
/// This system extracts all components of type `T` with an [`Animator<T>`]
/// attached to the same entity, and tick the animator to animate the component.
fn component_animator_system<T: Component>(
    time: Res<Time<Simulation>>,
    mut query: Query<(Entity, &mut T, &mut SimAnimator<T>)>,
    events: ResMut<Events<TweenCompleted>>,
    mut commands: Commands,
) {
    let mut events: Mut<Events<TweenCompleted>> = events.into();
    for (entity, target, mut animator) in query.iter_mut() {
        if animator.state != AnimatorState::Paused {
            let speed = animator.speed;
            let mut target = ComponentTarget::new(target);
            animator.tweenable.tick(
                time.delta().mul_f32(speed),
                &mut target,
                entity,
                &mut events,
                &mut commands,
            );
        }
    }
}
