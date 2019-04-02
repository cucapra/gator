precision mediump float;
attribute vec3 aPosition;
varying vec3 vPosition;
attribute vec3 aNormal;
varying vec3 vNormal;
varying vec3 vShadowPos;
uniform mat4 uModel;
uniform mat4 uCamera;
uniform mat4 uProjection;
uniform mat4 uLightView;
uniform mat4 uLightProjection;

void main() {
    mat4 texUnitConverter = mat4(0.5, 0., 0., 0., 0., 0.5, 0., 0., 0., 0., 0.5, 0., 0.5, 0.5, 0.5, 1.);
    vec4 worldPos = uModel*vec4(aPosition, 1.);
    vShadowPos = (texUnitConverter * uLightProjection * uLightView * worldPos).xyz;
    vPosition = aPosition;
    vNormal = aNormal;
    gl_Position = uProjection * uCamera * worldPos;
}