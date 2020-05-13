precision mediump float;
uniform mat4 uModel;
uniform mat4 uView;
varying vec3 vNormal;
uniform vec3 uLight;
varying vec3 vPosition;
void main() {
    vec3 ambient = vec3(.1, 0., 0.);
    vec3 diffuse = vec3(0.004, 0.003, 0.009);
    vec3 spec = vec3(.01, 0.01, 0.01);
    vec4 homWorldPos = uModel*vec4(vPosition, 1.0);
    vec3 camPos = normalize(vec3(uView*homWorldPos));
    vec3 worldNorm = normalize(vec3(uModel*vec4(vNormal, 0.0)));
    float lambertian=0.;
    float specular = 0.;
    for (int x=-5; x<=5; x++) {
        for (int y=-5; y<=5; y++) {
            vec3 lightDir = normalize(uLight  + vec3(1.,1.,1.) - vec3(homWorldPos));
            float lightWorldDot = dot(lightDir, worldNorm);
            vec3 reflectDir =  2.0*lightWorldDot*worldNorm - lightDir;
            lambertian += max(lightWorldDot, 0.0);
            specular += pow(max(-dot(camPos, reflectDir), 0.), 32.);

        }
    }
    gl_FragColor = vec4(ambient+lambertian*diffuse+specular*spec, 1.0);
}