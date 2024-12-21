use bevy::prelude::*;

use crate::{CELL_SIZE, CENTIMETER};

#[derive(Bundle)]
pub struct RobotBundle {
    location: Transform,
    mesh: Mesh2d,
    material: MeshMaterial2d<ColorMaterial>,
    robot_state: RobotState,
}

pub const ROBOT_WIDTH: f32 = 0.9 * (CELL_SIZE);

impl RobotBundle {
    pub fn new(
        location: Vec2,
        meshes: &mut Assets<Mesh>,
        materials: &mut Assets<ColorMaterial>,
    ) -> Self {
        Self {
            location: Transform::from_translation((location * CELL_SIZE).extend(0.0)),
            mesh: Mesh2d(meshes.add(Triangle2d::new(
                Vec2::Y * ROBOT_WIDTH / 2.0,
                Vec2::new(-ROBOT_WIDTH / 2.0, -ROBOT_WIDTH / 2.0),
                Vec2::new(ROBOT_WIDTH / 2.0, -ROBOT_WIDTH / 2.0),
            ))),
            material: MeshMaterial2d(materials.add(ColorMaterial::from(Color::hsl(0.0, 1.0, 1.0)))),
            robot_state: RobotState {},
        }
    }
}

#[derive(Component)]
pub struct RobotState {}

pub struct RobotBehaviorPlugin;

impl Plugin for RobotBehaviorPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, setup_system);
        app.add_systems(FixedUpdate, robot_behavior);
    }
}

fn setup_system(mut commands: Commands) {}

fn robot_behavior(mut robot_query: Query<(&mut Transform, &mut RobotState)>) {
    for (mut transform, mut robot_state) in robot_query.iter_mut() {
        // Increase the X by 0.1 every frame
        transform.translation.x += 0.1;
    }
}
