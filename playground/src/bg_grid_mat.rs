use bevy::{prelude::*, render::render_resource::AsBindGroup, sprite::Material2d};

#[derive(Asset, TypePath, AsBindGroup, Debug, Clone)]
pub struct GridMaterial {
    #[uniform(0)]
    pub(crate) thin_color: Vec4,
    #[uniform(1)]
    pub(crate) thick_color: Vec4,
    #[uniform(2)]
    pub(crate) bg_color: Vec4,
    #[uniform(3)]
    pub(crate) size: Vec4,
    #[uniform(4)]
    pub(crate) pan: Vec4,
}

pub const THIN_LINE: Color = Color::rgba(0.5, 0.5, 0.5, 0.2);
pub const THICK_LINE: Color = Color::rgba(0.0, 0.0, 0.0, 1.0);

impl Default for GridMaterial {
    fn default() -> Self {
        Self {
            thin_color: THIN_LINE.to_linear().to_vec4(),
            thick_color: THICK_LINE.to_linear().to_vec4(),
            bg_color: Color::NONE.to_linear().to_vec4(),
            size: Vec4::ONE,
            pan: Vec4::ZERO,
        }
    }
}

impl Material2d for GridMaterial {
    fn fragment_shader() -> bevy::render::render_resource::ShaderRef {
        "shaders/grid-shader.wgsl".into()
    }
    fn alpha_mode(&self) -> bevy::sprite::AlphaMode2d {
        bevy::sprite::AlphaMode2d::Blend
    }
}
