#version 150

uniform sampler2D tex;
in vec2 texCoordFrag;
out vec4 fragColor;

// TODO: add uniform color to enable setting the font color and also transparency
// uniform vec4 color;

void main() {
  fragColor = texture(tex, texCoordFrag);
}
