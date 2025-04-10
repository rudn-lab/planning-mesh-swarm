use bevy::{prelude::*, render::render_resource::AsBindGroup, sprite::Material2d};

#[derive(Asset, TypePath, AsBindGroup, Debug, Clone)]
pub(crate) struct GridMaterial {
    #[uniform(0)]
    pub(crate) line_color: Vec4,
    #[uniform(1)]
    pub(crate) bg_color: Vec4,
    #[uniform(2)]
    pub(crate) num_cells: Vec4,
    #[uniform(3)]
    pub(crate) line_thickness_minmax: Vec4,
    // #[uniform(4)]
    // pub(crate) line_thickness_max: f32,
    #[uniform(4)]
    pub(crate) cursor_position: Vec4,
    #[uniform(5)]
    pub(crate) spotlight_radius: Vec4,
}

impl Default for GridMaterial {
    fn default() -> Self {
        Self {
            line_color: Color::BLACK.to_linear().to_vec4(),
            bg_color: Color::NONE.to_linear().to_vec4(),
            num_cells: Default::default(),
            line_thickness_minmax: Vec4::new(1.0, 1.0, 0.0, 0.0),
            cursor_position: Vec4::default(),
            spotlight_radius: Default::default(),
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
