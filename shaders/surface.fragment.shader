#version 330 core

in vec2 texture_coordinates;
in float green;
out vec4 color;

uniform sampler2D rho_sampler;
uniform float max_amplitude;
uniform float amplitude;

void main()
{
    float rho = texture(rho_sampler, texture_coordinates).r;

    // Goes from -1 to 1
    float normalized_rho = rho * (amplitude / max_amplitude);

    float color_range = 0.3;
    float red = 0.5 + ( color_range * normalized_rho ) / 2;

    color = vec4(red, green, 0.0, 1.0);
}
