// Based on https://www.shadertoy.com/view/7tGBDK
// ..and https://ruby0x1.github.io/machinery_blog_archive/post/borderland-between-rendering-and-editor-part-1/index.html

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
var<uniform> thin_color: vec4<f32>;
@group(2) @binding(1)
var<uniform> thick_color: vec4<f32>;
@group(2) @binding(2)
var<uniform> bg_color: vec4<f32>;
@group(2) @binding(3)
var<uniform> size: vec4<f32>;
@group(2) @binding(4)
var<uniform> pan: vec4<f32>;



fn max2(v: vec2<f32>) -> f32 {
    return max(v.x, v.y);
}

fn log10(x: f32) -> f32 {
    return log(x) / log(10.0);
}

fn _mod(x: vec2<f32>, y: f32) -> vec2<f32> {
    return x - y * floor(x / y);
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    let grid_size = 200.0;
    var minCellSize = 0.01;
    var minCellPixelWidth = 2.0;

    let uv = (pan.xy/size.xy) + (in.uv * grid_size);
    var dudv = vec2(length(vec2(dpdx(uv.x), dpdy(uv.x))),length(vec2(dpdx(uv.y), dpdy(uv.y))));

    let lod: f32 = max(0.0, log10((max2(dudv) * minCellPixelWidth) / minCellSize) + 1.0);
    let fade: f32 = fract(lod);

    let lod0: f32 = minCellSize * pow(10.0, floor(lod));
    let lod1: f32 = lod0 * 10.0;
    let lod2: f32 = lod1 * 10.0;

    let line_width = 2.0;

    let lod0a: f32 = max2(vec2<f32>(1.0) - abs(clamp(_mod(uv, lod0) / dudv / line_width, vec2<f32>(0.0), vec2<f32>(1.0)) * 2.0 - vec2<f32>(1.0)));
    let lod1a: f32 = max2(vec2<f32>(1.0) - abs(clamp(_mod(uv, lod1) / dudv / line_width, vec2<f32>(0.0), vec2<f32>(1.0)) * 2.0 - vec2<f32>(1.0)));
    let lod2a: f32 = max2(vec2<f32>(1.0) - abs(clamp(_mod(uv, lod2) / dudv / line_width, vec2<f32>(0.0), vec2<f32>(1.0)) * 2.0 - vec2<f32>(1.0)));

    var v1: vec4<f32> = thin_color;
    var v2: f32 = lod1a * (1.0 - fade);
    if (lod2a > 0.0) { v1 = thick_color; } else if (lod1a > 0.0) { v1 = mix(thick_color, thin_color, fade); };
    if (lod2a > 0.0) { v2 = lod2a; } else if (lod1a > 0.0) { v2 = lod1a; }; 

    return vec4<f32>(v1 *  v2);
}