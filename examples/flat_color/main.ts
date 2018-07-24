import * as lgl from '../lglexample';
import { check_null } from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';
import * as model3D from 'teapot';

import shaderData from './data.json';

function main() {
  let canvas = document.getElementById('c') as HTMLCanvasElement;
  let gl = lgl.setup(canvas, render);

  let program = lgl.compileProgram(gl, shaderData.vertex, shaderData.fragment);

  // Uniform locations.
  let loc_uProjection = check_null(gl.getUniformLocation(program, 'uProjection'));
  let loc_uView = check_null(gl.getUniformLocation(program, 'uView'));
  let loc_uModel = check_null(gl.getUniformLocation(program, 'uModel'));

  // Attribute locations.
  let loc_aPosition = check_null(gl.getAttribLocation(program, 'aPosition'));
  let loc_aNormal = check_null(gl.getAttribLocation(program, 'aNormal'));

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

    let count = model3D.cells.length * model3D.cells[0].length;
    gl.drawElements(gl.TRIANGLES, count, gl.UNSIGNED_SHORT, 0);
  }
}

main();
