use bevy::{prelude::*, render::render_resource::AsBindGroup, sprite::Material2d};

use crate::CENTIMETER;

#[derive(Asset, TypePath, AsBindGroup, Debug, Clone)]
pub(crate) struct AntennaReachVisualizationMaterial {
    #[uniform(0)]
    pub(crate) radius: f32,
    #[uniform(1)]
    pub(crate) center: Vec2,
    #[uniform(2)]
    pub(crate) logistic_x0: f32,
    #[uniform(3)]
    pub(crate) logistic_k: f32,
    #[uniform(4)]
    pub(crate) threshold: f32,
}

impl Default for AntennaReachVisualizationMaterial {
    fn default() -> Self {
        Self {
            radius: 10.0 * CENTIMETER,
            center: Vec2::ZERO,
            logistic_x0: 0.0,
            logistic_k: -3.0,
            threshold: 0.05,
        }
    }
}

impl Material2d for AntennaReachVisualizationMaterial {
    fn fragment_shader() -> bevy::render::render_resource::ShaderRef {
        "shaders/antenna-reach-vis.wgsl".into()
    }

    fn alpha_mode(&self) -> bevy::sprite::AlphaMode2d {
        bevy::sprite::AlphaMode2d::Blend
    }
    fn depth_bias(&self) -> f32 {
        -1.0
    }
}
