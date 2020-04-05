precision mediump float; vec3 hom_reduce_1(vec4 v){return vec3(v);}vec3 hom_reduce_0(vec4 v){return (vec3(v)/v[3]);}vec4 homify_1(vec3 v){return vec4(v, 0.);}vec4 homify_0(vec3 v){return vec4(v, 1.);}mat4 _gator_times_30(mat4 m2, mat4 m1){return (m2*m1);}mat4 _gator_plus_18(mat4 m1, mat4 m2){return (m1+m2);}vec4 _gator_times_29(mat4 m, vec4 p){return (m*p);}vec4 _gator_times_28(mat4 m, vec4 v){return (m*v);}vec4 _gator_minus_20(vec4 x, vec4 y){return ((x*y[3])-(y*x[3]));}vec4 _gator_plus_17(vec4 v, vec4 p){return (p+(v*p[3]));}vec4 _gator_plus_16(vec4 p, vec4 v){return (p+(v*p[3]));}vec3 reflect_5(vec3 v1, vec3 v2){return reflect(v1, v2);}vec3 reflect_4(vec3 v1, vec3 v2){return reflect(v1, v2);}vec3 normalNormalize_1(vec3 v){return normalize(v);}vec3 normalize_5(vec3 v){return v;}vec3 normalize_4(vec3 v){return normalize(v);}float length_4(vec3 v){return length(v);}float dot_4(vec3 v1, vec3 v2){return dot(v1, v2);}mat3 _gator_times_27(mat3 m2, mat3 m1){return (m2*m1);}mat3 _gator_plus_15(mat3 m1, mat3 m2){return (m1+m2);}vec3 _gator_times_26(mat3 m, vec3 d){return (m*d);}vec3 _gator_times_25(mat3 m, vec3 p){return (m*p);}vec3 _gator_times_24(mat3 m, vec3 v){return (m*v);}vec3 _gator_minus_19(vec3 v){return -v;}vec3 _gator_minus_18(vec3 x, vec3 y){return (x-y);}vec3 _gator_plus_14(vec3 v, vec3 p){return (p+v);}vec3 _gator_plus_13(vec3 p, vec3 v){return (p+v);}vec3 _gator_minus_17(vec3 v){return -v;}vec3 _gator_minus_16(vec3 v){return -v;}vec3 _gator_times_23(float s, vec3 v){return (s*v);}vec3 _gator_times_22(vec3 v, float s){return (v*s);}vec3 _gator_minus_15(vec3 x, vec3 y){return (x-y);}vec3 _gator_plus_12(vec3 x, vec3 y){return (x+y);}vec2 mix_2(vec2 v1, vec2 v2, vec2 v3){return mix(v1, v2, v3);}vec2 reflect_3(vec2 v1, vec2 v2){return reflect(v1, v2);}vec2 reflect_2(vec2 v1, vec2 v2){return reflect(v1, v2);}vec2 normalNormalize_0(vec2 v){return normalize(v);}vec2 normalize_3(vec2 v){return v;}vec2 normalize_2(vec2 v){return normalize(v);}float length_3(vec2 v){return length(v);}float dot_3(vec2 v1, vec2 v2){return dot(v1, v2);}mat2 _gator_times_21(mat2 m2, mat2 m1){return (m2*m1);}mat2 _gator_plus_11(mat2 m1, mat2 m2){return (m1+m2);}vec2 _gator_times_20(mat2 m, vec2 p){return (m*p);}vec2 _gator_times_19(mat2 m, vec2 p){return (m*p);}vec2 _gator_times_18(mat2 m, vec2 v){return (m*v);}vec2 _gator_minus_14(vec2 v){return -v;}vec2 _gator_minus_13(vec2 x, vec2 y){return (x-y);}vec2 _gator_plus_10(vec2 v, vec2 p){return (p+v);}vec2 _gator_plus_9(vec2 p, vec2 v){return (p+v);}vec2 _gator_minus_12(vec2 v){return -v;}vec2 _gator_minus_11(vec2 v){return -v;}vec2 _gator_times_17(float s, vec2 v){return (s*v);}vec2 _gator_times_16(vec2 v, float s){return (v*s);}vec2 _gator_minus_10(vec2 x, vec2 y){return (x-y);}vec2 _gator_plus_8(vec2 x, vec2 y){return (x+y);}varying vec3 vPosition;uniform mat4 uModel;uniform mat4 uView;varying vec3 vNormal;uniform vec3 uLight;uniform float uRef;void main(){vec3 ambient = vec3(0.1, 0., 0.);vec3 diffColor = vec3(0.2, 0.8, 0.4);vec3 specColor = vec3(1., 1., 1.);if ((uRef==1.)){ mat3 modelMatrix = mat3(uModel);vec3 worldPos = vec3((uModel*vec4(-vPosition, 1.)));vec3 worldNorm = vec3((uModel*vec4(vNormal, 0.)));vec3 lightDir = normalize((uLight-worldPos));float lightWorldDot = dot(lightDir, worldNorm);float diffuse = max(lightWorldDot, 0.);vec3 camPos = vec3((uView*vec4(worldPos, 1.)));vec3 reflectDir = vec3((uView*vec4(reflect(-lightDir, worldNorm), 0.)));float specular = pow(max(dot(normalize(-camPos), reflectDir), 0.), 32.);gl_FragColor = vec4(((ambient+(diffuse*diffColor))+(specular*specColor)), 1.);}else { mat3 modelMatrix = mat3(uModel);vec3 worldPos = vec3((uModel*vec4(vPosition, 1.)));vec3 worldNorm = vec3((uModel*vec4(vNormal, 0.)));vec3 lightDir = normalize((uLight-worldPos));float lightWorldDot = dot(lightDir, worldNorm);float diffuse = max(lightWorldDot, 0.);vec3 camPos = vec3((uView*vec4(worldPos, 1.)));vec3 reflectDir = vec3((uView*vec4(reflect(-lightDir, worldNorm), 0.)));float specular = pow(max(dot(normalize(-camPos), reflectDir), 0.), 32.);gl_FragColor = vec4(((ambient+(diffuse*diffColor))+(specular*specColor)), 1.);}}