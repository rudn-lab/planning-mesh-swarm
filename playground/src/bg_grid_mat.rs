use bevy::{prelude::*, render::render_resource::AsBindGroup, sprite::Material2d};

#[derive(Asset, TypePath, AsBindGroup, Debug, Clone)]
pub struct GridMaterial {
    #[uniform(0)]
    pub(crate) line_color: Vec4,
    #[uniform(1)]
    pub(crate) bg_color: Vec4,
    #[uniform(2)]
    pub(crate) num_cells: Vec2,
    #[uniform(3)]
    pub(crate) line_thickness: f32,
}

impl Default for GridMaterial {
    fn default() -> Self {
        Self {
            line_color: Color::BLACK.to_linear().to_vec4(),
            bg_color: Color::NONE.to_linear().to_vec4(),
            num_cells: Vec2::default(),
            line_thickness: f32::default(),
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
