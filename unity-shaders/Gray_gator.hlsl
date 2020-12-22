#ifndef GRAYSCALE_INCLUDED 
#define GRAYSCALE_INCLUDED 

float2 _gator_plus_6(float2 x, float2 y){return (x+y);}

float2 _gator_minus_10(float2 x, float2 y){return (x-y);}

float2 _gator_times_12(float2 v, float s){return (v*s);}

float2 _gator_times_13(float s, float2 v){return (s*v);}

float2 _gator_minus_11(float2 v){return -v;}

float2 _gator_minus_12(float2 v){return -v;}

float2 _gator_plus_7(float2 p, float2 v){return (p+v);}

float2 _gator_plus_8(float2 v, float2 p){return (v+p);}

float2 _gator_minus_13(float2 p, float2 v){return (p-v);}

float2 _gator_minus_14(float2 v, float2 p){return (v-p);}

float2 _gator_minus_15(float2 x, float2 y){return (x-y);}

float2 _gator_minus_16(float2 v){return -v;}

float dot_3(float2 v1, float2 v2){return dot(v1, v2);}

float length_3(float2 v){return length(v);}

float distance_3(float2 p1, float2 p2){return distance(p1, p2);}

float2 normalize_2(float2 v){return normalize(v);}

float2 normalize_3(float2 v){return v;}

float2 normalNormalize(float2 v){return normalize(v);}

float2 reflect_2(float2 v1, float2 v2){return reflect(v1, v2);}

float2 reflect_3(float2 v1, float2 v2){return reflect(v1, v2);}

float2 lerp_2(float2 v1, float2 v2, float2 v3){return lerp(v1, v2, v3);}

float3 _gator_plus_9(float3 x, float3 y){return (x+y);}

float3 _gator_minus_17(float3 x, float3 y){return (x-y);}

float3 _gator_times_14(float3 v, float s){return (v*s);}

float3 _gator_times_15(float s, float3 v){return (s*v);}

float3 _gator_minus_18(float3 v){return -v;}

float3 _gator_minus_19(float3 v){return -v;}

float3 _gator_plus_10(float3 p, float3 v){return (p+v);}

float3 _gator_plus_11(float3 v, float3 p){return (v+p);}

float3 _gator_minus_20(float3 p, float3 v){return (p-v);}

float3 _gator_minus_21(float3 v, float3 p){return (v-p);}

float3 _gator_minus_22(float3 x, float3 y){return (x-y);}

float3 _gator_minus_23(float3 v){return -v;}

float dot_4(float3 v1, float3 v2){return dot(v1, v2);}

float length_4(float3 v){return length(v);}

float distance_4(float3 p1, float3 p2){return distance(p1, p2);}

float3 normalize_4(float3 v){return normalize(v);}

float3 normalize_5(float3 v){return v;}

float3 reflect_4(float3 v1, float3 v2){return reflect(v1, v2);}

float3 reflect_5(float3 v1, float3 v2){return reflect(v1, v2);}

float4 _gator_plus_12(float4 p, float4 v){return (p+(v*p[3]));}

float4 _gator_plus_13(float4 v, float4 p){return (p+(v*p[3]));}

float4 _gator_minus_24(float4 x, float4 y){return ((x*y[3])-(y*x[3]));}

float4 _gator_times_16(float4x4 m, float4 v){return mul(v, m);}

float4 _gator_times_17(float4x4 m, float4 v){return mul(v, m);}

float4 _gator_times_18(float4x4 m, float4 p){return mul(p, m);}

float4x4 _gator_plus_14(float4x4 m1, float4x4 m2){return (m1+m2);}

float4x4 _gator_times_19(float4x4 m2, float4x4 m1){return (m2*m1);}

float4 homify(float3 v){return float4(v[0], v[1], v[2], 1.);}

float4 homify_1(float3 v){return float4(v[0], v[1], v[2], 0.);}

float4 homify_2(float3 v){return float4(v[0], v[1], v[2], 0.);}

float3 hom_reduce(float4 v){return float3((v[0]/v[3]), (v[1]/v[3]), (v[2]/v[3]));}

float3 hom_reduce_1(float4 v){return float3(v[0], v[1], v[2]);}

void Grayscale_float(float3 inp, out float result){float r = inp[0];float g = inp[1];float b = inp[2];result = (((r+g)+b)*0.33);}

#endif