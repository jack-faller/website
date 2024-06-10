#version 330 core

uniform mat4 perspective;
uniform mat4 transform;

// Vertex input should be a polygon covering a circle of radius 1 centred on (0, 0).
layout (location = 0) in vec2 vertex;
// This gives the centre of the sphere, and the forth component is radius.
layout (location = 1) in vec4 offset;

out vec4 centre;
out vec4 position;
out float radius;

void main() {
  // Note: could do some complicated stuff with determinate of transform here to
  // get proper radius, but it's not worth it.
  radius = offset.w;
  centre = transform * vec4(offset.xyz, 1.0);
  position = centre + vec4(vertex * radius, radius, 0.0);
  gl_Position = perspective * position;
}
