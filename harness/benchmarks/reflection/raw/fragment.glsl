precision mediump float;

varying vec3 vPosition;
varying vec3 vNormal;

uniform samplerCube uSkybox;
uniform mat4 uView;
uniform mat4 uModel;
uniform mat3 uNormalMatrix;
uniform mat3 uInverseViewTransform;

void main() {
    vec3 N = normalize(uNormalMatrix * vNormal);
    vec3 V = -vec3(uView * uModel * vec4(vPosition, 1.));
    vec3 R = -reflect(V, N);
    gl_FragColor = textureCube(uSkybox, uInverseViewTransform * R);
}