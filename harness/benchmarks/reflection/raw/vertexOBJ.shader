precision mediump float;

attribute vec3 aPosition;
attribute vec3 aNormal;
varying vec3 vPosition;
varying vec3 vNormal;
uniform mat4 uProjection;
uniform mat4 uView;
uniform mat4 uModel;

vec4 homify(vec3 v){return vec4(v, 1.);}

void main() {
    vNormal = aNormal;
    vPosition = aPosition;
    gl_Position = (uProjection * ((uView * ((uModel * (homify(aPosition)))))));
}