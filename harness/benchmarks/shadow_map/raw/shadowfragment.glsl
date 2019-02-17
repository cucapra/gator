precision mediump float;
// varying vec3 vPosition;
vec4 encodeFloat(float depth){
    vec4 bitShift = vec4(256. * 256. * 256., 256. * 256., 256., 1.);
    vec4 bitMask = vec4(0, 1.,1, 1.) / 256.;
    vec4 comp = fract(depth * bitShift);
    comp -= comp.xxyz * bitMask;
    return comp;
}
void main() {
    gl_FragColor = encodeFloat(gl_FragCoord.z);
}