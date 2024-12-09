struct VertexOutput {
    // this is `clip position` when the struct is used as a vertex stage output 
    // and `frag coord` when used as a fragment stage input
    @builtin(position) position: vec4<f32>,
    @location(0) world_position: vec4<f32>,
    @location(1) world_normal: vec3<f32>,
    @location(2) uv: vec2<f32>,
    #ifdef VERTEX_TANGENTS
    @location(3) world_tangent: vec4<f32>,
    #endif
    #ifdef VERTEX_COLORS
    @location(4) color: vec4<f32>,
    #endif
}

@group(2) @binding(0)
var<uniform> line_color: vec4<f32>;
@group(2) @binding(1)
var<uniform> bg_color: vec4<f32>;
@group(2) @binding(2)
var<uniform> num_cells: vec2<f32>;
@group(2) @binding(3)
var<uniform> line_thickness: f32;

const pi = 3.14159265359;

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
  var out = bg_color;
  var f_before = sin(pi * (num_cells * in.uv - line_thickness));
  var f_after = sin(pi * (num_cells * in.uv + line_thickness));
  var sign_before = sign(f_before);
  var sign_after = sign(f_after);
  if (sign_before.x != sign_after.x) {out = line_color;};
  if (sign_before.y != sign_after.y) {out = line_color;};
  return vec4<f32>(out);
}
