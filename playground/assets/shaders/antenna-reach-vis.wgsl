#import bevy_sprite::mesh2d_vertex_output::VertexOutput

@group(2) @binding(0)
var<uniform> radius: f32;
@group(2) @binding(1)
var<uniform> center: vec2<f32>;
@group(2) @binding(2)
var<uniform> threshold: f32;
@group(2) @binding(3)
var<uniform> camera_scale: f32;
@group(2) @binding(4)
var<uniform> is_selected: f32;
@group(2) @binding(5)
var<uniform> shade_color: vec4<f32>;
@group(2) @binding(6)
var<uniform> time: f32;

fn signal_strength(rel_distance: f32) -> f32 {
  var value = max(0.0, 1 - rel_distance * rel_distance);
  value = value * value * value;

  return value;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
  var out = vec4<f32>(1.0, 0.0, 1.0, 1.0);

  out.x = (center - in.world_position.xy).x;
  out.y = (center - in.world_position.xy).y;

  // Distance function
  var dist = length(center - in.world_position.xy);

  dist /= radius;

  // If the distance is greater than the radius, return transparency
  if (dist > 1.0) {
    return vec4<f32>(0.0, 0.0, 0.0, 0.0);
  }

  var value = signal_strength(dist);

  if (is_selected == 1.0) {
    // Draw a line at the threshold:
    // if the value is close to it,
    // return yellow

    // get the angle of the current point relative to the center point
    var angle = atan2(in.world_position.x - center.x, in.world_position.y - center.y);
    var wave = sin((angle * 20.0) + time);

    var epsilon = 0.002 * camera_scale;
    if (abs(value - threshold * camera_scale) < epsilon) {
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