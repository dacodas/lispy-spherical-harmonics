#version 330 core

in vec2 texture_coordinates;
out vec4 color;

uniform sampler2D rho_sampler;
uniform float screen_scale;

void main()
{
    float rho = texture(rho_sampler, vec2(texture_coordinates.x * screen_scale, texture_coordinates.y)).r;

    color = vec4( rho, rho, rho, 1.0);
}
