precision mediump float;
uniform mat4 uModel;
uniform mat4 uView;
varying vec3 vNormal;
uniform vec3 uLight;
varying vec3 vPosition;
void main() {
    vec3 ambient = vec3(.1, 0., 0.);
    vec3 lightColor = vec3(0.4, 0.3, 0.9);

    vec3 worldPos = vec3(uModel*vec4(vPosition, 1.0));
    vec3 worldNorm = normalize(vec3(uModel*vec4(vNormal, 0.0)));

    vec3 lightDir = normalize(uLight - worldPos);
    vec3 reflectDir = reflect(-lightDir, vNormal);

    vec3 diffuse = max(dot(worldNorm, lightDir), 0.0) * lightColor;

    float spec = pow(max(dot(lightDir, reflectDir), 0.), 32.);
    vec3 specular = spec * vec3(1., 1., 1.);

    vec3 result = ambient + diffuse + specular;
    gl_FragColor = vec4(result, 1.0);
}