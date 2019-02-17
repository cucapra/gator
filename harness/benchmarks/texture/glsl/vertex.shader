precision mediump float;

attribute vec3 aPosition;
uniform mat4 uProjection;
uniform mat4 uView;
uniform mat4 uModel;
attribute vec2 aTexCoord;
varying vec2 vTexCoord;

void main() {
    v3_v4 = mat4(1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 1., 0., 0., 0., 0., 0.);
    vTexCoord = aTexCoord;
    gl_Position = uProjection * uView * uModel * vec4(aPosition, 1.);
}