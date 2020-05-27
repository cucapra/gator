precision mediump float;

attribute vec3 aPosition;
uniform mat4 uProjection;
uniform mat4 uView;
uniform mat4 uModel;
varying vec3 vPosition;

void main() {
    vPosition = aPosition;
    gl_Position = uProjection * uView * uModel * vec4(aPosition, 1.);
}