use std::collections::HashSet;

use bevy::{prelude::*, sprite::Material2dPlugin};

use crate::bg_grid_mat::{self, GridMaterial};

#[derive(Component, Hash, PartialEq, Eq, Debug, Reflect)]
pub struct GridMarker {}

pub struct Grid;

impl Plugin for Grid {
    fn build(&self, app: &mut App) {
        app.add_plugins(Material2dPlugin::<bg_grid_mat::GridMaterial>::default())
            .add_systems(Startup, prepare_grid)
            .add_systems(Update, update_grid);
    }
}

#[derive(Resource, Copy, Clone, Default)]
pub struct GridMaterialId(AssetId<GridMaterial>);

fn prepare_grid(
    mut commands: Commands,
    mut materials: ResMut<Assets<GridMaterial>>,
    mut meshes: ResMut<Assets<Mesh>>,
) {
    log::info!("Preparing grid");
    let mat = materials.add(GridMaterial { ..default() });

    commands.insert_resource(GridMaterialId(mat.id()));
    commands
        .spawn(GridMarker {})
        .insert(Transform::default().with_scale(Vec3::splat(10.0)))
        .insert(MeshMaterial2d(mat))
        .insert(Mesh2d(meshes.add(Rectangle::new(100.0, 100.0))))
        .insert(Name::new("GridQuad"));
}

pub fn update_grid(
    mut commands: Commands,
    mut grid_query: Query<(&mut Transform, &mut MeshMaterial2d<GridMaterial>), With<GridMarker>>,
    camera_query: Query<(&Transform, &Camera, &OrthographicProjection), (Without<GridMarker>)>,
    mut materials: ResMut<Assets<GridMaterial>>,
    mat_id: Res<GridMaterialId>,
) {
    let (camera_transform, camera, projection) = camera_query.single();

    // Update the entity's transform to match the camera's XY
    let (x, y) = (
        camera_transform.translation.x,
        camera_transform.translation.y,
    );
    let (mut grid, mut grid_material) = grid_query.single_mut();
    grid.translation.x = x;
    grid.translation.y = y;
    if let Some(prev) = materials.get(mat_id.0) {
        let mut mat = prev.clone();
        mat.pan.x = (x / 2.0);
        mat.pan.y = (-y / 2.0);
        mat.size = Vec4::splat(projection.scale);
        materials.insert(mat_id.0, mat);
    }
}
