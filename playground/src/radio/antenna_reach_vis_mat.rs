use bevy::{
    math::VectorSpace, prelude::*, render::render_resource::AsBindGroup, sprite::Material2d,
};

use crate::CENTIMETER;

#[derive(Asset, TypePath, AsBindGroup, Debug, Clone)]
pub(crate) struct AntennaReachVisualizationMaterial {
    #[uniform(0)]
    pub(crate) radius: Vec4,
    #[uniform(1)]
    pub(crate) center: Vec4,
    #[uniform(2)]
    pub(crate) threshold: Vec4,
    #[uniform(3)]
    pub(crate) camera_scale: Vec4,

    /// Value is either 0.0 or 1.0
    #[uniform(4)]
    pub(crate) is_selected: Vec4,

    #[uniform(5)]
    pub(crate) shade_color: Vec4,

    #[uniform(6)]
    pub(crate) time: Vec4,
}

impl Default for AntennaReachVisualizationMaterial {
    fn default() -> Self {
        Self {
            radius: Vec2::new(10.0 * CENTIMETER, 0.0).extend(0.0).extend(0.0),
            center: Vec2::ZERO.extend(0.0).extend(0.0),
            threshold: Vec4::ZERO,
            camera_scale: Vec4::ONE,
            is_selected: Vec4::ZERO,
            shade_color: Vec4::new(0.0, 1.0, 0.0, 0.5),
            time: Vec4::ZERO,
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
