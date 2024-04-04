#version 330 core

layout (location = 0) out vec4 colour;

uniform sampler2D normals;
uniform vec4 light_direction;
// Each value in the dither map describes the minimum light level that pixel
// will be white.
const int dither_length = 5;
uniform float dither_map[dither_length * dither_length];

// float rand(vec2 co){
//     return fract(sin(dot(co, vec2(12.9898, 78.233))) * 43758.5453);
// }

const float normal_simlarity = cos(radians(13));

vec4 read_normal(ivec2 size, vec2 pos) {
  return normalize(texture(normals, pos / size));
}

void main() {
  ivec2 size = textureSize(normals, 0);
  vec4 normal = read_normal(size, gl_FragCoord.xy);
  vec4 normal_left = read_normal(size, gl_FragCoord.xy + vec2(1, 0));
  vec4 normal_down = read_normal(size, gl_FragCoord.xy + vec2(0, 1));
  colour = vec4(vec3(0), 1);
  float light_level = dot(normal, light_direction) / 2 + 0.5;
  // Background gets no light.
  if (normal == vec4(vec3(0), 1))
	light_level = 0;
  ivec2 coords = ivec2(trunc(mod(gl_FragCoord.xy, dither_length)));
  // colour = vec4(vec3(light_level > dither_map[coords.y * dither_length + coords.x]), 1);
  colour = vec4(vec3(light_level), 1);
  // Angles less than ~7 degrees don't count as edges.
  if (dot(normal_down, normal) < normal_simlarity || dot(normal_left, normal) < normal_simlarity)
	colour = vec4(vec3(0), 1);
}
