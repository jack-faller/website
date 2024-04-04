#version 330 core

layout (location = 0) out vec4 colour;


uniform sampler2D normals;
uniform vec4 light_direction;
// Each value in the dither map describes the minimum light level that pixel
// will be white.
uniform sampler2D dither_map;

in vec4 position;

const float normal_simlarity = cos(radians(13));

vec4 read_normal(ivec2 size, vec2 pos) {
  return normalize(texture(normals, pos / size));
}

void main() {
  ivec2 size = textureSize(normals, 0);
  vec4 normal = read_normal(size, gl_FragCoord.xy);
  vec4 normal_left = read_normal(size, gl_FragCoord.xy + vec2(1, 0));
  vec4 normal_down = read_normal(size, gl_FragCoord.xy + vec2(0, 1));
  float light_level = dot(normal, light_direction);
  // Background gets no light.
  if (normal == vec4(vec3(0), 1))
	light_level = 0;
  // colour = vec4(vec3(light_level > texture(dither_map, position.xy).x), 1);
  colour = vec4(vec3(light_level), 1);
  // Edge detection.
  if (dot(normal_down, normal) < normal_simlarity || dot(normal_left, normal) < normal_simlarity)
	colour = vec4(vec3(0), 1);
}
