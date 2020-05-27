precision mediump float;

attribute vec3 aPosition;
attribute vec3 aNormal;
attribute vec2 aTexCoord;
attribute vec3 aTangent;
attribute vec3 aBitangent;
varying vec3 geom_position;
varying vec2 geom_texCoord;
varying vec3 geom_normal;
varying vec3 geom_tangent;
varying vec3 geom_bitangent;
uniform mat4 uProjection;
uniform mat4 uView;
uniform mat4 uModel;

void main() {
  geom_position = vec3(uView * uModel * vec4(aPosition, 1.));
  geom_normal = vec3(uView * uModel * vec4(aNormal, 0.));
  geom_texCoord = aTexCoord;
  geom_tangent = vec3(uView * uModel * vec4(aTangent, 1.));
  geom_bitangent = vec3(uView * uModel * vec4(aBitangent, 1.));
  gl_Position = uProjection * uView * uModel * vec4(aPosition, 1.);
}