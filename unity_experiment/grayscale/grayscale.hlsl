#ifndef GRAYSCALE_INCLUDED
#define GRAYSCALE_INCLUDED

void Grayscale_float(float3 input, out float output)
{
    float r = input[0];
    float g = input[1];
    float b = input[2];
    output = (r + g + b) * 0.33;
}

#endif