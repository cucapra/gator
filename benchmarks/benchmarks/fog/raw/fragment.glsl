precision mediump float;

// Global Variables
varying vec3 vPosition;
uniform mat4 uModel;
uniform mat4 uView;
varying vec3 vNormal;
uniform vec3 uLight;
uniform vec3 uViewPoint;
uniform vec3 diffColor;
uniform mat4 uLightTrans;

void main() {
    vec3 ambient = vec3(.1, 0.1, 0.1);
    vec3 specColor = vec3(1.0, 1.0, 1.0);
    vec3 worldPos = vec3(uModel * vec4(vPosition, 1.));
    vec3 camPos = vec3(uView * vec4(worldPos, 1.));
    vec3 worldNorm = normalize(vec3(uModel * vec4(vNormal, 0.)));
    vec3 lightDir = normalize((vec3(uLightTrans * vec4(uLight, 1.))) - worldPos);
    float lightWorldDot = dot(lightDir, worldNorm);
    float diffuse = max(lightWorldDot, 0.0);
    vec3 reflectDir = normalize(vec3(uView * vec4(reflect(-lightDir, worldNorm), 0.)));
    float specular = pow(max(dot(normalize(-camPos), reflectDir), 0.), 32.);
    //worldPos
    vec3 lightVec = vec3(uLightTrans * vec4(uLight, 1.)) - worldPos;//vector
    vec3 viewPos = vec3(uLightTrans * vec4(uViewPoint, 1.));
    vec3 BA = lightVec - worldPos;
    vec3 BC = viewPos - worldPos;
    float bcMagnitude = length(BC);
    float H = length(cross(BA, BC));
    float fogVal = atan(bcMagnitude, H)/H;
    float fogStr = 100.0;
    float r = fogVal*fogStr;
    vec3 rvec = vec3(r,r,r);
    gl_FragColor = vec4(ambient + (diffuse * diffColor + specular * specColor)*r, 1.0);
}