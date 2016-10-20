#version 150
in vec2 position;
uniform vec2 offset;

void main() {
  gl_Position = vec4(offset + 0.1*position, 1.0, 1.0);
}
