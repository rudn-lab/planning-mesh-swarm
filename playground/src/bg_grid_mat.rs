use bevy::{prelude::*, render::render_resource::AsBindGroup, sprite::Material2d};

#[derive(Asset, TypePath, AsBindGroup, Debug, Clone)]
pub(crate) struct GridMaterial {
    #[uniform(0)]
    pub(crate) line_color: Vec4,
    #[uniform(1)]
    pub(crate) bg_color: Vec4,
    #[uniform(2)]
    pub(crate) num_cells: Vec2,
    #[uniform(3)]
    pub(crate) line_thickness_min: f32,
    #[uniform(4)]
    pub(crate) line_thickness_max: f32,
    #[uniform(5)]
    pub(crate) cursor_position: Vec2,
    #[uniform(6)]
    pub(crate) spotlight_radius: f32,
}

impl Default for GridMaterial {
    fn default() -> Self {
        Self {
            line_color: Color::BLACK.to_linear().to_vec4(),
            bg_color: Color::NONE.to_linear().to_vec4(),
            num_cells: Vec2::default(),
            line_thickness_min: f32::default(),
            line_thickness_max: f32::default(),
            cursor_position: Vec2::default(),
            spotlight_radius: f32::default(),
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
