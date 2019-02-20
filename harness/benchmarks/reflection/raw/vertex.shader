precision mediump float;

attribute vec3 aPosition;
attribute vec3 aNormal;
uniform mat4 uProjection;
uniform mat4 uView;
uniform mat4 uModel;
varying vec3 vPosition;
varying vec3 vNormal;

void main() {
    mat4 model_modelHom = mat4(1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 0.);
    vPosition = aPosition;
    vNormal = aNormal;
    gl_Position = uProjection * uView * uModel * (model_modelHom * vec4(aPosition, 0.) + vec4(0., 0., 0., 1.));
}