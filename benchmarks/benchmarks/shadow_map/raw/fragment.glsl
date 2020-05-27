precision mediump float;
uniform sampler2D uTexture;
varying vec3 vShadowPos;
varying vec3 vPosition;
uniform mat4 uModel;
uniform mat4 uCamera;
varying vec3 vNormal;
uniform vec3 uLight;
uniform vec3 uBaseColor;
uniform float uSpecStrength;
vec4 homify(vec3 v){return vec4(v, 1.); }
vec3 hom_reduce(vec4 v){return vec3(v); }
vec4 homify_normal(vec3 v){return vec4(v, 0.); }
vec4 extendColor(vec3 v){return vec4(v, 1.); }

float decodeFloat(vec4 c){
    vec4 bitShift = vec4(1. / (256. * 256. * 256.), 1. / (256. * 256.), 1. / 256., 1.);
    return dot(c, bitShift);
}

vec3 phong_light(vec3 lightPos, vec3 fragPos, vec3 normalDir, vec3 baseColor,
 float specStrength, float linear, float quad){
    vec3 normLightPos = normalize(lightPos);

    float lambertian = max(0., dot(normalDir, normLightPos));
    vec3 reflectDir = reflect(-normLightPos, normalDir);
    float specular = 0.;
    if (0. <= lambertian){
        specular = pow(max(0., -dot(normalize(fragPos), reflectDir)), 32.);
    }
    float distance = length(lightPos - fragPos);
    float attenuation = 1. / (linear * distance + quad * distance * distance);
    return (lambertian*baseColor + specStrength * specular * vec3(1., 1., 1.)) * attenuation;
}

void main() {
    vec3 ambient = vec3(.1, 0., 0.);
    float texelSize = (1. / 1024.);
    vec4 v = texture2D(uTexture, vShadowPos.xy);
    float amountInLight = 0.;
    for (int x = -1; x <= 1; x++){
        for (int y = -1; y <= 1; y++){
            float texelDepth = decodeFloat(texture2D(uTexture, vShadowPos.xy + vec2(x, y) * texelSize));
            if (vShadowPos.z - 0.007 <= texelDepth){
                amountInLight += 1.;
            }
        }
    }
    amountInLight /= 9.;
    vec3 phong_color = phong_light(uLight, (uModel * homify(vPosition)).xyz, normalize((uModel * homify_normal(vNormal)).xyz), uBaseColor, uSpecStrength, 0.08, 0.0001);
    gl_FragColor = extendColor(ambient + phong_color * amountInLight);
}