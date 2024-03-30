#version 330 core

uniform mat4 perspective;
uniform mat4 transform;

layout(location = 0) in vec3 vertex;
layout(location = 1) in vec3 normal_in;

out vec4 normal;

void main() {
  gl_Position = perspective * transform * vec4(vertex, 1.0);
  normal = normalize(transform * vec4(normal_in, 0));
}
