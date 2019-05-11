#version 330 core

in vec2 vertex_position;
out vec2 texture_coordinates;

void main()
{
    gl_Position = vec4(vertex_position / 3 + vec2(0.5, 0.5), 0, 1);
    texture_coordinates = vertex_position;
}
