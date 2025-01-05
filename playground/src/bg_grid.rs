use bevy::{prelude::*, sprite::Material2dPlugin};

use crate::bg_grid_mat::{self, GridMaterial};

#[derive(Component, Hash, PartialEq, Eq, Debug, Reflect)]
pub(crate) struct GridMarker {}

pub(crate) struct Grid {
    pub(crate) mesh_size: Vec2,
    pub(crate) num_cells: Vec2,
    pub(crate) line_thickness: f32,
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
    line_thickness: f32,
    line_color: Color,
    bg_color: Color,
}

impl Plugin for Grid {
    fn build(&self, app: &mut App) {
        app.add_plugins(Material2dPlugin::<bg_grid_mat::GridMaterial>::default())
            .insert_resource(GridConfig {
                mesh_size: self.mesh_size,
                num_cells: self.num_cells,
                line_thickness: self.line_thickness,
                line_color: self.line_color,
                bg_color: self.bg_color,
            })
            .add_systems(Startup, prepare_grid);
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
        line_thickness: grid_config.line_thickness,
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
