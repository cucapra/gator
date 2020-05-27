precision mediump float;
precision mediump int;

const float PI = 3.14159265359;
varying vec3 geom_position;
varying vec2 geom_texCoord;
varying vec3 geom_normal;
varying vec3 geom_tangent;
varying vec3 geom_bitangent;
uniform vec3 mat_diffuseColor;
uniform int mat_hasDiffuseTexture;
uniform sampler2D mat_diffuseTexture;
uniform float mat_indexOfRefraction;
uniform float mat_alpha;
uniform int mat_hasAlphaTexture;
uniform sampler2D mat_alphaTexture;
uniform int mat_hasNormalTexture;
uniform sampler2D mat_normalTexture;
uniform float mat_normalTextureScale;
uniform vec3 light_eyePosition;
uniform vec3 light_attenuation;
uniform vec3 light_color;

vec3 proj_normlocalframe (vec3 m, vec3 n) {
    vec3 nhat = normalize(n);
    vec3 t = normalize(geom_tangent - dot(geom_tangent,nhat));
    vec3 b = cross(nhat, t);
    if (dot(geom_bitangent, b) <= 0.) {
        b = -b;
    }
    return vec3(dot(m, t), dot(m,b), dot(m,nhat));
}

vec3 getShadingNormal() {
    vec3 nhat = normalize(geom_normal);
    if (mat_hasNormalTexture == 1) {
        vec3 rgb = vec3(texture2D(mat_normalTexture, geom_texCoord));
        nhat = vec3(2.*rgb[0] - 1., 2.*rgb[1] - 1., 2. * rgb[2] - 1.);

        vec3 n = normalize(geom_normal);
        vec3 t = normalize(geom_tangent - dot(geom_tangent,n) * n);
        vec3 b = cross(n, t);
        if (dot(geom_bitangent, b) < 0.) {
            b = -b;
        }

        float s = mat_normalTextureScale;

        return normalize(s * nhat[0] * t + s * nhat[1] * b + nhat[2] * n);
    }
    else {return nhat;}
}

vec3 get_halfdir(vec3 n1, vec3 n2){
	return normalize(n1 + n2);
}

vec4 alphatize(vec3 c)  {return vec4(c, 1.);}

float F(vec3 i, vec3 m) {
	float c = abs(dot(i, m));
	float g = sqrt(mat_indexOfRefraction * mat_indexOfRefraction - 1. + c * c);

	return (1. / 2.) * ((g-c)*(g-c)) / ((g+c)*(g+c)) * 
    (1. + ((c * (g + c) - 1.)*(c * (g + c) - 1.)) / ((c * (g - c) + 1.)*(c * (g - c) + 1.)));
}

float getRoughness() {
	float a = mat_alpha;
	if (mat_hasAlphaTexture == 1){
		vec4 temp = texture2D(mat_alphaTexture, geom_texCoord);
		a *= temp[0];
	}
	return a;
}

float getAngle(vec3 v1, vec3 v2) { 
	return acos(dot(v1, v2));
}

float D(vec3 m, vec3 n) {
	if ((dot(m, n) > 0.)) {  
		float roughness = getRoughness();
		float theta = getAngle(m, n);
		return (roughness*roughness) / (PI * (cos(theta)*cos(theta)*cos(theta)*cos(theta))
      * (((roughness*roughness) + (tan(theta)*tan(theta)))*((roughness*roughness) + (tan(theta)*tan(theta)))));
	}
	else { 
		return 0.;
	}
}

float G1(vec3 v, vec3 m, vec3 n) { 
  if (dot(v, m) * dot(v, n) > 0.) { 
	  float roughness = getRoughness();
	  float theta = getAngle(v, normalize(n));
	  return (2. / (1. + sqrt(1. + (roughness*roughness) * (tan(theta)*tan(theta)))));
  }
  else { return 0.; }
}

float G(vec3 i, vec3 o, vec3 m, vec3 n) {
  return (G1(i, m, getShadingNormal()) * G1(o, m, getShadingNormal()));
}

vec3 getDiffuseColor() {
  vec3 kd = mat_diffuseColor;
  if (mat_hasDiffuseTexture == 1) { 
    vec4 tmp = texture2D(mat_diffuseTexture, geom_texCoord);
    kd[0] *= tmp[0];
    kd[1] *= tmp[1];
    kd[2] *= tmp[2];
  }
  return kd;
}

vec3 I(vec3 c, vec3 source, vec3 target, vec3 a) { 
  float d = distance(source, target);
  return (c / (a[0] + a[1] * d + a[2] * d * d));
}

vec3 promote(float f) {
  return vec3(f, f, f);
}

void main() {
  	vec4 final_color = vec4(0., 0., 0., 1.);
	vec3 i = normalize(light_eyePosition - geom_position);
	vec3 o = normalize(-geom_position);
	vec3 m = get_halfdir(i, o);
	vec3 n = getShadingNormal();
	vec3 kd = getDiffuseColor();
	final_color += alphatize(
		(kd + promote(F(i,m) * D(m,n) * G(i,o,m,n) / (4.0 * abs (dot(i,n)) * abs(dot(o,n))))) * max(0., dot(n,i)) 
		* I(light_color, light_eyePosition, geom_position, light_attenuation));
	gl_FragColor = final_color;
}