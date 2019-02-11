precision mediump float;

attribute vec3 aPosition;
attribute vec3 aNormal;
varying vec3 vPosition;
varying vec3 vNormal;
uniform mat4 uProjection;
uniform mat4 uView;
uniform mat4 uModel;

void main() {
    mat4 v3_v4 = mat4(1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 0.);
    vNormal = aNormal;
    vPosition = aPosition;
    gl_Position = (((((uProjection * uView)) * uModel)) * (((v3_v4 * vec4(aPosition, 0.)) + vec4(0., 0., 0., 1.))));
}
