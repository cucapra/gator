precision mediump float;
attribute vec3 aPosition;
varying vec3 vPosition;
uniform mat4 uModel;
uniform mat4 uLightView;
uniform mat4 uLightProjection;
vec4 homify(vec3 v){return vec4(v, 1.); }
vec3 hom_reduce(vec4 v){return vec3(v); }
void main() {
    // Don't think this is necessary?
    // vPosition = hom_reduce(uLightProjection * uLightView * uModel * vec4(aPosition, 1.));
    gl_Position = uLightProjection * uLightView * uModel * vec4(aPosition, 1);
}