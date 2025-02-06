use bevy::{prelude::*, render::render_resource::AsBindGroup, sprite::Material2d};

use crate::CENTIMETER;

#[derive(Asset, TypePath, AsBindGroup, Debug, Clone)]
pub(crate) struct AntennaReachVisualizationMaterial {
    #[uniform(0)]
    pub(crate) radius: f32,
    #[uniform(1)]
    pub(crate) center: Vec2,
    #[uniform(2)]
    pub(crate) threshold: f32,
    #[uniform(3)]
    pub(crate) camera_scale: f32,

    /// Value is either 0.0 or 1.0
    #[uniform(4)]
    pub(crate) is_selected: f32,

    #[uniform(5)]
    pub(crate) shade_color: Vec4,

    #[uniform(6)]
    pub(crate) time: f32,
}

impl Default for AntennaReachVisualizationMaterial {
    fn default() -> Self {
        Self {
            radius: 10.0 * CENTIMETER,
            center: Vec2::ZERO,
            threshold: 0.05,
            camera_scale: 1.0,
            is_selected: 0.0,
            shade_color: Vec4::new(0.0, 1.0, 0.0, 0.5),
            time: 0.0,
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
