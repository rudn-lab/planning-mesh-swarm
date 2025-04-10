#import bevy_sprite::mesh2d_vertex_output::VertexOutput

@group(2) @binding(0)
var<uniform> radius: vec4<f32>; // f32 with padding
@group(2) @binding(1)
var<uniform> center: vec4<f32>; // vec2<f32> with padding
@group(2) @binding(2)
var<uniform> threshold: vec4<f32>; // f32 with padding
@group(2) @binding(3)
var<uniform> camera_scale: vec4<f32>; // f32 with padding
@group(2) @binding(4)
var<uniform> is_selected: vec4<f32>; // f32 with padding
@group(2) @binding(5)
var<uniform> shade_color: vec4<f32>;
@group(2) @binding(6)
var<uniform> time: vec4<f32>; // f32 with padding

fn signal_strength(rel_distance: f32) -> f32 {
  var value = max(0.0, 1 - rel_distance * rel_distance);
  value = value * value * value;

  return value;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
  var out = vec4<f32>(1.0, 0.0, 1.0, 1.0);

  out.x = (center.xy - in.world_position.xy).x;
  out.y = (center.xy - in.world_position.xy).y;

  // Distance function
  var dist = length(center.xy - in.world_position.xy);

  dist /= radius.x;

  // If the distance is greater than the radius, return transparency
  if (dist > 1.0) {
    return vec4<f32>(0.0, 0.0, 0.0, 0.0);
  }

  var value = signal_strength(dist);

  if (is_selected.x == 1.0) {
    // Draw a line at the threshold:
    // if the value is close to it,
    // return yellow

    // get the angle of the current point relative to the center point
    var angle = atan2(in.world_position.x - center.x, in.world_position.y - center.y);
    var wave = sin((angle * 20.0) + time.x);

    var epsilon = 0.00005 * camera_scale.x;
    if (abs(value - threshold.x * camera_scale.x) < epsilon) {
      if (wave > 0.0) {
        out = vec4<f32>(1.0, 1.0, 0.0, 1.0);
        return out;
      }
    }    
  }

  out = shade_color;

  out.a *= value;


  return out;
}