precision mediump float;

attribute vec3 aPosition;
attribute vec3 aNormal;
uniform mat4 uProjection;
uniform mat4 uView;
uniform mat4 uModel;
varying vec3 vPosition;
varying vec3 vNormal;

void main() {
    vPosition = aPosition;
    vNormal = aNormal;
    gl_Position = uProjection * uView * uModel * vec4(aPosition, 1.);
}