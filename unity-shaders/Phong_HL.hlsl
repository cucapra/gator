#ifndef PHONG_INCLUDED
#define PHONG_INCLUDED

void Phong_float(float3 vPosition, float4x4 uModel, float4x4 uView, float3 vNormal, float3 uLight, float uRef, float4 output) {
    float3 ambient = {.1, 0., 0.};
    float3 diffColor = {0.2, 0.8, 0.4};
    float3 specColor = {1.0, 1.0, 1.0};

    float4 worldPosTemp1 = float4(-(vPosition.x), -(vPosition.y), -(vPosition.z), 0.0);
    float4 worldPosTemp2 = mul(worldPosTemp1, uModel);
    float3 worldPos = float3(worldPosTemp2.x, worldPosTemp2.y, worldPosTemp2.z);

    float4 worldNormTemp0 = float4(vNormal.x, vNormal.y, vNormal.z, 0.0);
    float4 worldNormTemp1 = mul(worldNormTemp0, uModel);
    float3 worldNorm = float3(worldNormTemp1.x, worldNormTemp1.y, worldNormTemp1.z);

    float3 lightDir = normalize(uLight - worldPos);
    float lightWorldDot = dot(lightDir, worldNorm);
    float diffuse = max(lightWorldDot, 0.0);

    float4 camPos1 = mul(float4(worldPos.x, worldPos.y, worldPos.z, 1.0), uView);
    float3 camPos = float3(camPos1.x, camPos1.y, camPos1.z);

    float3 reflDir0 = reflect((-lightDir), worldNorm);
    float4 reflectDir1 = mul(float4(reflDir0.x, reflDir0.y, reflDir0.z, 0.0), uView);
    float3 reflectDir = float3(reflectDir1.x, reflectDir1.y, reflectDir1.z);

    float specular = pow(max(dot(normalize(-camPos), reflectDir), 0.0), 32.0);

    float4 gl_FragColor = float4(ambient + mul(diffuse, diffColor) + mul(specular, specColor), 1.0);
    output = gl_FragColor;
}

#endif