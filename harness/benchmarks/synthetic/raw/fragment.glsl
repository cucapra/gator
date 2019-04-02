precision mediump float;
uniform mat4 uModel;
uniform mat4 uView;
varying vec3 vNormal;
uniform vec3 uLight0;
uniform vec3 uLight1;
uniform vec3 uLight2;
uniform vec3 uLight3;
uniform vec3 uLight4;
varying vec3 vPosition;
void main() {
    vec3 ambient = vec3(.1, 0., 0.);
    vec3 diffuse = 0.5*vec3(0.4, 0.3, 0.9);
    vec3 spec = vec3(1.0, 1.0, 1.0);

    vec4 homWorldPos = uModel*vec4(vPosition, 1.0);
    vec3 camPos = normalize(vec3(uView*homWorldPos));
    vec3 worldNorm = normalize(vec3(uModel*vec4(vNormal, 0.0)));

    vec3 lightPos = vec3(0.,0.,0.);
    vec3 lightDir = vec3(0.,0.,0.);
    float lightWorldDot = 0.;
    vec3 reflectDir = vec3(0.,0.,0.);
    float lambertian = 0.;
    float specular = 0.;

    lightPos = uLight0;
    lightDir = normalize(lightPos - vec3(homWorldPos));
    lambertian += max(dot(lightDir, worldNorm), 0.0);
    reflectDir = reflect(-lightDir, worldNorm);
    specular += pow(max(-dot(camPos, reflectDir), 0.), 32.);

    lightPos = uLight1;
    lightDir = normalize(lightPos - vec3(homWorldPos));
    lambertian += max(dot(lightDir, worldNorm), 0.0);
    reflectDir = reflect(-lightDir, worldNorm);
    specular += pow(max(-dot(camPos, reflectDir), 0.), 32.);

    lightPos = uLight2;
    lightDir = normalize(lightPos - vec3(homWorldPos));
    lambertian += max(dot(lightDir, worldNorm), 0.0);
    reflectDir = reflect(-lightDir, worldNorm);
    specular += pow(max(-dot(camPos, reflectDir), 0.), 32.);

    lightPos = uLight3;
    lightDir = normalize(lightPos - vec3(homWorldPos));
    lambertian += max(dot(lightDir, worldNorm), 0.0) ;
    reflectDir = reflect(-lightDir, worldNorm);
    specular += pow(max(-dot(camPos, reflectDir), 0.), 32.);

    lightPos = uLight4;
    lightDir = normalize(lightPos - vec3(homWorldPos));
    lambertian += max(dot(lightDir, worldNorm), 0.0);
    reflectDir = reflect(-lightDir, worldNorm);
    specular += pow(max(-dot(camPos, reflectDir), 0.), 32.);

    gl_FragColor = vec4(ambient + lambertian*diffuse + specular*spec, 1.0);
}