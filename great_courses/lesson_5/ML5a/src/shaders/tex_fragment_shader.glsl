#version 410 core

in vec2 v_texcoord;

uniform sampler2D texture2d;

out vec4 fragment_colour;

void main()
    {
    fragment_colour = texture(texture2d, v_texcoord);
    }
