#version 330 core

in vec3 normal;

out vec4 colour;

void main() {
  colour = vec4(normal, 1);
}
