use bevy::{prelude::*, sprite::Material2dPlugin, window::PrimaryWindow};

use crate::{
    bg_grid_mat::{self, GridMaterial},
    CELL_SIZE,
};

#[derive(Component, Hash, PartialEq, Eq, Debug, Reflect)]
pub(crate) struct GridMarker {}

pub(crate) struct Grid {
    pub(crate) mesh_size: Vec2,
    pub(crate) num_cells: Vec2,
    pub(crate) line_thickness_min: f32,
    pub(crate) line_thickness_max: f32,
    pub(crate) spotlight_radius: f32,
    pub(crate) line_color: Color,
    pub(crate) bg_color: Color,
}

// There's probably a better way to do it
#[derive(Resource, Debug, Clone, Copy, PartialEq)]
pub(crate) struct GridConfig {
    mesh_size: Vec2,
    num_cells: Vec2,
    /// Fraction of a square covered by line in both direction.
    /// So 0.5 will leave no background visible.
    line_thickness_min: f32,
    line_thickness_max: f32,
    spotlight_radius: f32,
    line_color: Color,
    bg_color: Color,
}

impl Plugin for Grid {
    fn build(&self, app: &mut App) {
        app.add_plugins(Material2dPlugin::<bg_grid_mat::GridMaterial>::default())
            .insert_resource(GridConfig {
                mesh_size: self.mesh_size,
                num_cells: self.num_cells,
                line_thickness_min: self.line_thickness_min,
                line_thickness_max: self.line_thickness_max,
                line_color: self.line_color,
                bg_color: self.bg_color,
                spotlight_radius: self.spotlight_radius,
            })
            .add_systems(Startup, prepare_grid)
            .add_systems(PostUpdate, update_grid_material);
    }
}

fn prepare_grid(
    mut commands: Commands,
    mut materials: ResMut<Assets<GridMaterial>>,
    mut meshes: ResMut<Assets<Mesh>>,
    grid_config: Res<GridConfig>,
) {
    log::info!("Preparing grid");
    let mat = materials.add(GridMaterial {
        line_color: grid_config.line_color.to_linear().to_vec4(),
        bg_color: grid_config.bg_color.to_linear().to_vec4(),
        num_cells: grid_config.num_cells,
        line_thickness_min: grid_config.line_thickness_min,
        line_thickness_max: grid_config.line_thickness_max,
        spotlight_radius: grid_config.spotlight_radius,
        cursor_position: Vec2::default(),
    });

    commands
        .spawn(GridMarker {})
        .insert(Transform::default().with_translation(Vec3::NEG_Z))
        .insert(MeshMaterial2d(mat))
        .insert(Mesh2d(meshes.add(Rectangle::new(
            grid_config.mesh_size.x,
            grid_config.mesh_size.y,
        ))))
        .insert(Name::new("GridQuad"));
}

pub(crate) fn update_grid_material(
    mut assets: ResMut<Assets<GridMaterial>>,
    mut query: Query<&mut MeshMaterial2d<GridMaterial>, With<GridMarker>>,
    camera: Query<(&Camera, &GlobalTransform), With<Camera2d>>,
    window: Query<&Window, With<PrimaryWindow>>,
) {
    let (camera, camera_transform) = camera.single();
    let window = window.single();
    let cursor_pos = window.cursor_position().map(|cursor_pos| {
        camera
            .viewport_to_world_2d(camera_transform, cursor_pos)
            .expect("Failed to convert cursor position to world space")
    });

    if let Some(cursor_pos) = cursor_pos {
        for mut material_component in query.iter_mut() {
            let material = assets.get_mut(&mut material_component.0).unwrap();
            material.cursor_position = cursor_pos;
        }
    }
}

/// Transform the grid position to the world position
pub(crate) fn grid_pos_to_world(grid: (i32, i32)) -> Vec2 {
    let (x, y) = grid;
    let (x, y) = (x as f32, y as f32);
    let (x, y) = (x * CELL_SIZE, y * CELL_SIZE);
    Vec2::new(x, y)
}

/// Transform the world position to the nearest grid position,
/// accounting for the fact that the grid cells are at the intersections
pub(crate) fn snap_world_to_grid(world: Vec2) -> (i32, i32) {
    let (x, y) = (world.x / CELL_SIZE, world.y / CELL_SIZE);
    let (x, y) = (x.round() as i32, y.round() as i32);
    (x, y)
}
