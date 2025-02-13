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
var<uniform> line_thickness_min: f32;
@group(2) @binding(4)
var<uniform> line_thickness_max: f32;
@group(2) @binding(5)
var<uniform> cursor_position: vec2<f32>;
@group(2) @binding(6)
var<uniform> spotlight_radius: f32;

const pi = 3.14159265359;

fn falloff(x: f32) -> f32 {
  var value = max(0.0, 1 - x * x);
  value = pow(value, 2.0/1.0);
  return value;
}

fn get_thickness_at(uv: vec2<f32>) -> f32 {
  let distance = length(cursor_position - uv);
  let relative_distance = distance / spotlight_radius;
  let factor = falloff(relative_distance);

  return mix(line_thickness_min, line_thickness_max, factor);
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    let coord = in.world_position.xy / num_cells;

    // Compute anti-aliased world-space grid lines
    let grid_regular = abs(fract(coord - 0.5) - 0.5);
    var fwid = fwidth(coord) * get_thickness_at(in.world_position.xy);

    let grid = grid_regular / fwid;
    let line = min(grid.x, grid.y);

    // Just visualize the grid lines directly
    var color = 1.0 - min(line, 1.0);

    // Apply gamma correction
    color = pow(color, 1.0 / 2.2);
    return mix(bg_color, line_color, color);
}
