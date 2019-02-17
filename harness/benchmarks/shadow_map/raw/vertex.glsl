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
vec4 homify(vec3 v){return vec4(v, 1.); }
vec3 hom_reduce(vec4 v){return vec3(v); }

void main() {
    mat4 texUnitConverter = mat4(0.5, 0., 0., 0., 0., 0.5, 0., 0., 0., 0., 0.5, 0., 0.5, 0.5, 0.5, 1.);
    vShadowPos = hom_reduce(texUnitConverter * uLightProjection * uLightView * uModel * homify(aPosition));
    vPosition = aPosition;
    vNormal = aNormal;
    gl_Position = uProjection * uCamera * uModel * homify(aPosition);
}