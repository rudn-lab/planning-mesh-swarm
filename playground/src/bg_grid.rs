use std::collections::HashSet;

use bevy::prelude::*;

#[derive(Component, Clone, Copy, Hash, PartialEq, Eq, Debug, Reflect)]
pub struct GridPosition {
    pub x: i32,
    pub y: i32,
}

#[derive(Bundle)]
pub struct GridSquare {
    pub transform: Transform,
}

impl GridSquare {
    pub fn new(x: f32, y: f32) -> Self {
        Self {
            transform: Transform::from_xyz(x, y, 0.0),
        }
    }
}

// Create new entities for the grid, delete any that are outside the camera bounds
pub fn update_grid(
    mut commands: Commands,
    mut grid_squares: Query<(Entity, &mut Transform, &GridPosition)>,
    camera_query: Query<(&Transform, &Camera, &OrthographicProjection), (Without<GridPosition>)>,
    mut materials: ResMut<Assets<ColorMaterial>>,
    mut meshes: ResMut<Assets<Mesh>>,
) {
    let (camera_transform, camera, projection) = camera_query.single();

    let x_top = camera_transform.translation.x + projection.area.min.x;
    let x_bottom = camera_transform.translation.x + projection.area.max.x;
    let y_top = camera_transform.translation.y + projection.area.min.y;
    let y_bottom = camera_transform.translation.y + projection.area.max.y;

    //log::info!("Bounds: {}, {}, {}, {}", x_top, x_bottom, y_top, y_bottom);

    let grid_step = 100.0;

    let grid_x_top = (x_top / grid_step).ceil() as i32;
    let grid_x_bottom = (x_bottom / grid_step).floor() as i32;
    let grid_y_top = (y_top / grid_step).ceil() as i32;
    let grid_y_bottom = (y_bottom / grid_step).floor() as i32;

    let mut grid_positions: Vec<GridPosition> = Vec::new();
    for (entity, mut transform, grid_position) in grid_squares.iter_mut() {
        if grid_position.x < grid_x_top
            || grid_position.x > grid_x_bottom
            || grid_position.y < grid_y_top
            || grid_position.y > grid_y_bottom
        {
            commands.entity(entity).despawn();
        }
        grid_positions.push(*grid_position);
    }

    let grid_positions = HashSet::<GridPosition>::from_iter(grid_positions.into_iter());

    for x in grid_x_top..=grid_x_bottom {
        for y in grid_y_top..=grid_y_bottom {
            if grid_positions.contains(&GridPosition { x, y }) {
                continue;
            }
            commands
                .spawn(GridSquare::new(x as f32 * grid_step, y as f32 * grid_step))
                .insert(GridPosition { x, y })
                .insert(Mesh2d(meshes.add(Rectangle::new(grid_step, grid_step))))
                .insert(MeshMaterial2d(materials.add(
                    Color::hsl(360. * x as f32 / 10.0, 1.0, 0.5).with_alpha(0.1),
                )));
        }
    }
}
