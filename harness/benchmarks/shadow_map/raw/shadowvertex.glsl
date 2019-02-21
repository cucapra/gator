precision mediump float;
attribute vec3 aPosition;
uniform mat4 uModel;
uniform mat4 uLightView;
uniform mat4 uLightProjection;
vec4 homify(vec3 v){return vec4(v, 1.); }
vec3 hom_reduce(vec4 v){return vec3(v); }
void main() {
    // Don't think this is necessary?
    gl_Position = uLightProjection * uLightView * uModel * vec4(aPosition, 1);
}