use std::time::Duration;

use bevy::prelude::*;
use bevy_tweening::{lens::TransformRotateZLens, Animator, RepeatCount, Tween};

use crate::CELL_SIZE;

#[derive(Bundle)]
pub struct ReticleBundle {
    mesh: Mesh2d,
    material: MeshMaterial2d<ColorMaterial>,
    transform: Transform,
    spin: Animator<Transform>,
}

impl ReticleBundle {
    pub fn new(asset_server: Res<AssetServer>, materials: &mut Assets<ColorMaterial>) -> Self {
        let handle = asset_server.load(
            GltfAssetLabel::Primitive {
                mesh: 0,
                primitive: 0,
            }
            .from_asset("selection-reticle.glb"),
        );
        // let handle = meshes.add(Annulus::new(90.0, 100.0));

        let tween = Tween::new(
            EaseFunction::Linear,
            Duration::from_secs_f32(1.0),
            TransformRotateZLens {
                start: 0.0,
                end: std::f32::consts::PI * 2.0,
            },
        )
        .with_repeat_strategy(bevy_tweening::RepeatStrategy::Repeat)
        .with_repeat_count(RepeatCount::Infinite);
        ReticleBundle {
            mesh: Mesh2d(handle),
            material: MeshMaterial2d(materials.add(ColorMaterial::from(
                // light blue
                Color::linear_rgb(0.0, 0.5, 0.8),
            ))),
            transform: Transform::from_scale(Vec3::splat(CELL_SIZE / 2.0)),

            spin: Animator::new(tween),
        }
    }
}
