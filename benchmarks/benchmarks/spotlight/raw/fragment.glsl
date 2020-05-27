precision mediump float;
uniform mat4 uModel;
uniform mat4 uView;
varying vec3 vNormal;
uniform vec3 uLight;
varying vec3 vPosition;
uniform mat4 uLightTrans;
void main() {
    vec3 ambient = vec3(.1, 0., 0.);
    vec3 diffColor = vec3(0.2, 0.8, 0.4);
    vec3 specColor = vec3(1.0, 1.0, 1.0);

    vec3 worldPos = vec3(uModel * vec4(vPosition, 1.));
    vec3 camPos = vec3(uView * vec4(worldPos, 1.));
    vec3 worldNorm = normalize(vec3(uModel * vec4(vNormal, 0.)));
    vec3 lightDir = normalize((vec3(uLightTrans * vec4(uLight, 1.))) - worldPos);
    float lightWorldDot = dot(lightDir, worldNorm);
    float diffuse = max(lightWorldDot, 0.0);
    vec3 reflectDir = normalize(vec3(uView * vec4(reflect(-lightDir, worldNorm), 0.)));
    float specular = pow(max(dot(normalize(-camPos), reflectDir), 0.), 32.);

    //Setup potlight pointing towards the world center at all times
    vec3 spotlightDir = normalize(-vec3(uLightTrans * vec4(uLight, 1.)));
    float thetaLimit = -0.999;
    float dirDot = dot(lightDir, spotlightDir);

    if(dirDot > thetaLimit){
        gl_FragColor = vec4(ambient, 1.0);
    }else{
        gl_FragColor = vec4(ambient + diffuse * diffColor + specular * specColor, 1.0);
    }
}
