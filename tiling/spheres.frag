#version 330 core

uniform mat4 perspective;
uniform mat4 transform;

in vec4 centre;
in vec4 position;
in float radius;

layout (location = 0) out vec4 colour;

void main() {
  vec2 xy = position.xy - centre.xy;
  float xy_len = length(xy);
  if (xy_len > radius)
	discard;
  vec4 sphere_pos = (centre + vec4(xy, -sqrt(radius * radius - xy_len * xy_len), 0.0));
  // colour = normalize(vec4((sphere_pos - centre).xyz, 0));
  colour = vec4(1, vec3(0));
  sphere_pos = perspective * sphere_pos;
  sphere_pos /= sphere_pos.w;
  // vec4 val = perspective * position;
  // if (position.z < 0) discard;
  // gl_FragDepth = sphere_pos.z;
}
