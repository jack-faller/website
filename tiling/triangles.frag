#version 330 core

in vec4 normal;

layout (location = 0) out vec4 colour;

void main() {
  colour = normal;
}
