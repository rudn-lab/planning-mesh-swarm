use bevy::{ecs::system::SystemId, prelude::*};
use bevy_tweening::{Animator, AnimatorState};

use crate::robot::motion_types::BusyRobot;

#[derive(Resource, Default)]
pub(crate) struct PauseState {
    pub(crate) paused: bool,
}

#[derive(Resource)]
pub(crate) struct PauseHandlerSystems {
    pub(crate) on_pause: SystemId,
    pub(crate) on_unpause: SystemId,
}

impl PauseHandlerSystems {
    /// Returns the SystemId of the system we need to run when the pause state changes.
    /// For example: if the pause state became true, we need to run the `on_pause` system.
    pub(crate) fn on_pause_state_changed(&self, new_state: bool) -> SystemId {
        if new_state {
            self.on_pause
        } else {
            self.on_unpause
        }
    }
}

impl FromWorld for PauseHandlerSystems {
    fn from_world(world: &mut World) -> Self {
        Self {
            on_pause: world.register_system(on_pause_logic),
            on_unpause: world.register_system(on_unpause_logic),
        }
    }
}

pub(crate) struct PausePlugin;

impl Plugin for PausePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Update, handle_spacebar_pause)
            .add_systems(PostUpdate, pause_overlay_scale)
            .init_resource::<PauseState>()
            .init_resource::<PauseHandlerSystems>();
    }
}

fn handle_spacebar_pause(
    mut pause_state: ResMut<PauseState>,
    input: Res<ButtonInput<KeyCode>>,
    handlers: Res<PauseHandlerSystems>,
    mut commands: Commands,
) {
    if input.just_pressed(KeyCode::Space) {
        pause_state.paused = !pause_state.paused;
        commands.run_system(handlers.on_pause_state_changed(pause_state.paused));
    }
}

#[derive(Component)]
struct PauseOverlayMarker;

fn pause_overlay_scale(
    mut query: Query<(&mut Transform, &PauseOverlayMarker)>,
    camera: Query<&OrthographicProjection, With<Camera2d>>,
) {
    // Scale the pause overlay to the size of the camera
    let camera = camera.single();
    for (mut transform, _) in query.iter_mut() {
        transform.scale = Vec3::splat(camera.scale);
    }
}

fn on_pause_logic(
    mut busy_robots: Query<&mut Animator<Transform>, With<BusyRobot>>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    camera: Query<Entity, With<Camera2d>>,
) {
    // For all busy robots, pause their animations
    for mut animator in busy_robots.iter_mut() {
        animator.state = AnimatorState::Paused;
    }

    // Spawn the pause overlay
    let camera_id = camera.single();
    let root = commands
        .spawn(PauseOverlayMarker)
        .insert(Transform::from_xyz(0., 0., 100.))
        // The rectangle needs to be big enough to cover the entire screen at zoom level 1.
        .insert(Mesh2d(
            meshes.add(Rectangle::new(100_000_000., 100_000_000.)),
        ))
        .insert(MeshMaterial2d(
            materials.add(ColorMaterial::from(Color::linear_rgba(0., 0., 0., 0.2))),
        ))
        .insert(PickingBehavior::IGNORE)
        .insert(Name::new("PauseOverlay"))
        .set_parent(camera_id)
        .id();

    // Spawn a circle in the middle of the screen
    commands
        .spawn(Mesh2d(meshes.add(Circle::new(100.))))
        .insert(MeshMaterial2d(
            materials.add(ColorMaterial::from(Color::linear_rgba(0., 0., 0., 0.5))),
        ))
        .insert(Name::new("PauseCircle"))
        .insert(PickingBehavior::IGNORE)
        .set_parent(root);

    // Spawn two vertical bars in the middle of the screen: these are the pause icon
    commands
        .spawn(Mesh2d(meshes.add(Rectangle::new(25., 75.))))
        .insert(MeshMaterial2d(
            materials.add(ColorMaterial::from(Color::linear_rgba(1., 1., 1., 0.4))),
        ))
        .insert(Name::new("PauseBar1"))
        .insert(Transform::from_xyz(-25., 0., 100.))
        .insert(PickingBehavior::IGNORE)
        .set_parent(root);

    commands
        .spawn(Mesh2d(meshes.add(Rectangle::new(25., 75.))))
        .insert(MeshMaterial2d(
            materials.add(ColorMaterial::from(Color::linear_rgba(1., 1., 1., 0.4))),
        ))
        .insert(Name::new("PauseBar2"))
        .insert(Transform::from_xyz(25., 0., 100.))
        .insert(PickingBehavior::IGNORE)
        .set_parent(root);
}

fn on_unpause_logic(
    mut busy_robots: Query<&mut Animator<Transform>, With<BusyRobot>>,
    mut commands: Commands,
    pause_overlay: Query<Entity, With<PauseOverlayMarker>>,
) {
    // For all busy robots, unpause their animations
    for mut animator in busy_robots.iter_mut() {
        animator.state = AnimatorState::Playing;
    }

    // Despawn the pause overlay
    for entity in pause_overlay.iter() {
        commands.entity(entity).despawn_recursive();
    }
}
