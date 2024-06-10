#version 330 core

uniform mat4 perspective;
uniform mat4 transform;

in vec4 centre;
in vec4 position;
in float radius;

layout (location = 0) out vec4 colour;

float frag_depth(vec4 clip_pos) {
  float far = gl_DepthRange.far;
  float near = gl_DepthRange.near;
  return (((far-near) * clip_pos.z / clip_pos.w) + near + far) / 2.0;
}

void main() {
  vec2 xy = position.xy - centre.xy;
  float xy_len = length(xy);
  if (xy_len > radius)
	discard;
  vec4 sphere_pos = vec4(xy, sqrt(radius * radius - xy_len * xy_len), 0.0);
  colour = normalize(sphere_pos);
  gl_FragDepth = frag_depth(perspective * (sphere_pos + centre));
}
