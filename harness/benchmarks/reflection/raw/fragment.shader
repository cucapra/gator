precision mediump float;

varying vec3 vPosition;
varying vec3 vNormal;
uniform samplerCube uSkybox;
uniform mat4 uView;
uniform mat4 uModel;
uniform mat3 uNormalMatrix;
uniform mat3 uInverseViewTransform;

vec4 homify(vec3 v){return vec4(v, 1.);}
hom_reduce(vec4 v){return vec3(v);}
vec4 homify_normal(vec3 v){return vec4(v, 0.);}
vec4 extendColor(vec3 v){return vec4(v, 1.)}

void main() {
    vec3 N = normalize(((uNormalMatrix * vNormal)));
    vec3 V = -(hom_reduce(((uView * ((uModel * (homify(vPosition))))))));
    vec3 R = -(reflect(V, N));
    gl_FragColor = textureCube(uSkybox, ((uInverseViewTransform * R)));
}