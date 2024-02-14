#version 330 core

layout(location = 0) in vec3 vertex;

void main() {
  gl_Position.xyz = vertex;
  gl_Position.w = 1.0;
}
