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

  // look up where the vertex data needs to go.
  let shape_buffers = lgl.mesh_buffers(gl, model3D);

  // Create the base matrices to be used
  // when rendering the object.
  let model = mat4.create();

  function render(view: mat4, projection: mat4) {
    mat4.rotateY(model, model, .01);

    // Tell it to use our program (pair of shaders)
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, model);

    // Set the attribute arrays.
    // Note that attributes not used in a shader do not have a bound location
    lgl.bind_attrib_buffer(gl, loc_aNormal, shape_buffers.normals);
    lgl.bind_attrib_buffer(gl, loc_aPosition, shape_buffers.positions);

    // And the element array.
    // TODO What is an element array?
    lgl.bind_element_buffer(gl, shape_buffers.cells);

    gl.drawElements(gl.TRIANGLES, shape_buffers.cell_count, gl.UNSIGNED_SHORT, 0);
  }
}

main();
