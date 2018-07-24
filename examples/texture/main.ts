import * as lgl from '../lglexample';
import { mat4 } from 'gl-matrix';
import * as model3D from 'teapot';

import shaderData from './data.json';

function main() {
  let canvas = document.getElementById('c') as HTMLCanvasElement;
  let gl = lgl.setup(canvas, render);

  let program = lgl.compileProgram(gl, shaderData.vertex, shaderData.fragment);

  // Uniform and attribute locations.
  let loc_uProjection = lgl.uniformLoc(gl, program, 'uProjection');
  let loc_uView = lgl.uniformLoc(gl, program, 'uView');
  let loc_uModel = lgl.uniformLoc(gl, program, 'uModel');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');

  // We'll draw a teapot.
  let mesh = lgl.getTeapot(gl);

  // Initialize the model position.
  let model = mat4.create();

  function render(view: mat4, projection: mat4) {
    mat4.rotateY(model, model, .01);

    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, model);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aNormal, mesh.normals);
    lgl.bind_attrib_buffer(gl, loc_aPosition, mesh.positions);

    // Draw the object.
    lgl.drawMesh(gl, mesh);
  }
}

main();
