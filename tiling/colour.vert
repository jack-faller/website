#version 330 core

layout(location = 0) in vec3 vertex;

out vec4 position;

void main() {
  gl_Position = position = vec4(vertex, 1.0);
}
