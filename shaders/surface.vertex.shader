#version 330 core

in vec3 vertex_position;
in vec3 model_position;

in float green_v;
out float green;

out vec2 texture_coordinates;

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

    // Might be nice to grab these from a texture rather than computing them every time
    // float x = rho * texture( x_sampler, vertex_position.xy ).r;
    // float y = rho * texture( y_sampler, vertex_position.xy ).r;
    // float z = rho * texture( z_sampler, vertex_position.xy ).r;

    float x = rho * sin( M_PI * vertex_position.x ) * cos( 2 * M_PI * vertex_position.y );
    float y = rho * sin( M_PI * vertex_position.x ) * sin( 2 * M_PI * vertex_position.y );
    float z = rho * cos( M_PI * vertex_position.x );

    // mat4 instanced_model = mat4(1.0, 0.0, 0.0, model_position.x,
    //                             0.0, 1.0, 0.0, model_position.y,
    //                             0.0, 0.0, 1.0, model_position.z,
    //                             0.0, 0.0, 0.0, 1.0);

    mat4 instanced_model = mat4(1.0, 0.0, 0.0, 0.0,
                                0.0, 1.0, 0.0, 0.0,
                                0.0, 0.0, 1.0, 0.0,
                                model_position.x, model_position.y, model_position.z, 1.0);

 

    // mat4 instanced_model = mat4(1.0, 0.0, 0.0, 0.0,
    //                             0.0, 1.0, 0.0, 0.0,
    //                             0.0, 0.0, 1.0, 0.0,
    //                             0.0, 0.0, 0.0, 1.0);

    gl_Position = proj * view * instanced_model * model * vec4( vec3(x, y, z), 1.0 );

    texture_coordinates = vertex_position.xy;
    green = green_v;
}
