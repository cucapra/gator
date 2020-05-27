precision mediump float;

attribute vec3 aPosition;
attribute vec3 aDerivU;
attribute vec3 aDerivV;
attribute vec3 aNormal;
attribute vec2 aUv;
uniform mat4 uProjection;
uniform mat4 uView;
uniform mat4 uModel;
uniform mat3 uNormal;

varying vec3 vPosition;
varying vec3 vDerivU;
varying vec3 vDerivV;
varying vec3 vNormal;
varying vec2 vUv;
varying mat3 vNormalMatrix;

void main() {
  gl_Position = uProjection * uView * uModel * vec4(aPosition, 1.);
  vUv = aUv;
  vNormal = aNormal;
  vPosition = vec3(uModel * vec4(aPosition, 1.));
  vNormalMatrix = uNormal;
  vDerivU = aDerivU;
  vDerivV = aDerivV;
}