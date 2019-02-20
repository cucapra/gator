precision mediump float;

varying vec3 vPosition;
uniform mat4 uModel;
uniform mat4 uView;
varying vec3 vNormal;
uniform vec4 uLight;

vec4 homify(vec3 v){return vec4(v, 1.);}
vec3 hom_reduce(vec4 v){return vec3(v);}
vec4 homify_normal(vec3 v){return vec4(v, 0.);}
vec4 extendColor(vec3 v){return vec4(v, 1.);}

void main() {
    vec3 ambient = vec3(0.1, 0., 0.);
    vec3 diffuse = vec3(0.4, 0.3, 0.9);
    vec3 spec = vec3(1., 1., 1.);
    vec3 lightDir = normalize(((hom_reduce(uLight) - hom_reduce(((uModel * (homify(vPosition))))))));
    float lambertian = max((dot((normalize((hom_reduce(((uModel * (homify_normal(vNormal)))))))), lightDir)), 0.);
    vec3 reflectDir = normalize((hom_reduce(((uView * (homify_normal((reflect((-(lightDir)), (normalize((hom_reduce(((uModel * (homify_normal(vNormal)))))))))))))))));
    float specular = pow((max((dot((-(normalize((hom_reduce(((uView * ((uModel * (homify(vPosition))))))))))), reflectDir)), 0.)), 32.);
    gl_FragColor = extendColor((((ambient + (lambertian * diffuse)) + (specular * spec))));
}