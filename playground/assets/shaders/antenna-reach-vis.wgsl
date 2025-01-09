#import bevy_sprite::mesh2d_vertex_output::VertexOutput

@group(2) @binding(0)
var<uniform> radius: f32;
@group(2) @binding(1)
var<uniform> center: vec2<f32>;
@group(2) @binding(2)
var<uniform> logistic_x0: f32;
@group(2) @binding(3)
var<uniform> logistic_k: f32;
@group(2) @binding(4)
var<uniform> threshold: f32;

fn sigmoid_unshifted(x: f32) -> f32 {
    var numerator = pow(x, logistic_k);
    var denominator = pow(x, logistic_k) + pow(1.0 - x, logistic_k);

    return numerator / denominator;
}

fn sigmoid(x: f32) -> f32 {
    // The function is defined as:
    // \frac{\left(x-x_{0}\right)^{k}}{\left(x-x_{0}\right)^{k}\ +\ \left(1-\left(x-x_{0}\right)\right)^{k}}
    //
    // When x_0 = 0 and k is negative, f(0)=1 and f(1)=0.
    // When x_0 is not zero, then the values are not exactly 0 and 1.
    // To correct for this, we scale the function output,
    // so that across the entire range of 0-1, the new function takes all the values between 0 and 1.
    
    var shift_x = x - logistic_x0;

    var maximum_value = 0.5;

    // If k is positive, then the function slopes up,
    // (the signal strength gets higher as you move further away from the source: the opposite of what you want).
    // So the maximum value to be found is at the right side of the function.
    if (logistic_k > 0.0) {
        maximum_value = sigmoid_unshifted(1.0 - logistic_x0);
    }

    // If k is negative, then the function slopes down,
    // so the maximum value is at the left side of the function.
    if (logistic_k < 0.0) {
        maximum_value = sigmoid_unshifted(-logistic_x0);
    }
    
    // To scale it back up, divide by the greatest value achievable in the range.
    var scaled_value = sigmoid_unshifted(shift_x) / maximum_value;

    return scaled_value;
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

  var value = sigmoid_unshifted(dist);

  // If the value is above the threshold, set output to green
  if (value > threshold) {
    out = vec4<f32>(0.0, 1.0, 0.0, 0.2);
  }

  // If the value is close to the threshold, set output to yellow
  if (abs(value - threshold) < 0.01) {
    out = vec4<f32>(1.0, 1.0, 0.0, 1.0);
  }

  // If the value is below the threshold, set output to red
  if (value < threshold) {
    out = vec4<f32>(1.0, 0.0, 0.0, 0.2);
  }

  return out;
}