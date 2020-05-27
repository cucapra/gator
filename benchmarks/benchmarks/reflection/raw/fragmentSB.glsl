precision mediump float;

varying vec3 vPosition;
uniform samplerCube uSkybox;

void main() {
    gl_FragColor = textureCube(uSkybox, vPosition);
}