import {vec2,mat2,vec3,mat3,vec4,mat4} from 'gl-matrix';
import * as lgl from '../lglexample'; var gl;
var program;
var loc_uProjection;
var loc_uView;
var loc_uModel;
var loc_uLight;
var loc_aPosition;
var loc_aNormal;
var model;
var light;
var mesh;
function render_0(view:mat4, projection:mat4):void{
(mat4.rotateY(model, model, 0.1));
}
function main():void{
let gl = lgl.setup(render_0);
program=(lgl.compileProgram(gl, require("./vertex.lgl"), require("./fragment.lgl")));
let fs=(require("fs"));
loc_uProjection=(lgl.uniformLoc(gl, program, "uProjection"));
loc_uView=(lgl.uniformLoc(gl, program, "uView"));
loc_uModel=(lgl.uniformLoc(gl, program, "uModel"));
loc_uLight=(lgl.uniformLoc(gl, program, "uLight"));
loc_aPosition=(lgl.attribLoc(gl, program, "aPosition"));
loc_aNormal=(lgl.attribLoc(gl, program, "aNormal"));
mesh=(lgl.getBunny(gl));
model=(mat4.create());
light=(vec3.fromValues(20., 0., 20.));
}
main();