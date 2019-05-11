#version 330 core

in vec3 vertex_position;

uniform float amplitude;

uniform sampler2D rho_sampler_vertex;
uniform sampler2D x_sampler;
uniform sampler2D y_sampler;
uniform sampler2D z_sampler;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

#define M_PI 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679

void main()
{
    float rho = 1 + amplitude * ( 2 * texture( rho_sampler_vertex, vertex_position.xy ).r - 1 );

    float x = rho * sin( M_PI * vertex_position.x ) * cos( 2 * M_PI * vertex_position.y );
    float y = rho * sin( M_PI * vertex_position.x ) * sin( 2 * M_PI * vertex_position.y );
    float z = rho * cos( M_PI * vertex_position.x );

    gl_Position = proj * view * model * vec4( vec3(x, y, z), 1.0 );
}
