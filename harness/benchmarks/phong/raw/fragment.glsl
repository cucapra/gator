precision mediump float;
uniform mat4 uModel;
uniform mat4 uView;
varying vec3 vNormal;
uniform vec3 uLight;
varying vec3 vPosition;
void main() {
    vec3 ambient = vec3(.1, 0., 0.);
    vec3 lightColor = vec3(0.4, 0.3, 0.9);

    vec4 homWorldPos = uModel*vec4(vPosition, 1.0);
    vec3 camPos = normalize(vec3(uView*homWorldPos));
    vec3 worldNorm = normalize(vec3(uModel*vec4(vNormal, 0.0)));

    vec3 lightDir = normalize(uLight - vec3(homWorldPos));
    float lightWorldDot = dot(lightDir, worldNorm);
    // We might want to go back to using built in functions here.
    vec3 reflectDir =  2.0*lightWorldDot*worldNorm - lightDir;

    vec3 diffuse = max(lightWorldDot, 0.0) * lightColor;

    float spec = pow(max(-dot(camPos, reflectDir), 0.), 32.);
    vec3 specular = spec * vec3(1., 1., 1.);

    gl_FragColor = vec4(ambient+diffuse+specular, 1.0);
}