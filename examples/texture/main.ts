import * as lgl from '../lglexample';
import { check_null } from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';
import * as model3D from 'teapot';

import shaderData from './data.json';

function main() {
  let canvas = document.getElementById('c') as HTMLCanvasElement;
  let gl = lgl.setup(canvas, render);

  let program = lgl.compileProgram(gl, shaderData.vertex, shaderData.fragment);

  let uniformLocations: { [key: string]: WebGLUniformLocation } = {
    'uProjection': check_null(gl.getUniformLocation(program, 'uProjection')),
    'uView': check_null(gl.getUniformLocation(program, 'uView')),
    'uModel': check_null(gl.getUniformLocation(program, 'uModel')),
  };

  let attributeLocations: { [key: string]: number } = {
    'aPosition': check_null(gl.getAttribLocation(program, 'aPosition')),
    'aNormal': check_null(gl.getAttribLocation(program, 'aNormal')),
  };

  // look up where the vertex data needs to go.
  let shape_buffers = lgl.mesh_buffers(gl, model3D);

  // Create the base matrices to be used
  // when rendering the object.
  let model = mat4.create();

  // Clear the canvas
  gl.clearColor(0, 0, 0, 0);
  gl.clear(gl.COLOR_BUFFER_BIT);

  function render(view: mat4, projection: mat4) {
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(uniformLocations.uProjection, false, projection);
    gl.uniformMatrix4fv(uniformLocations.uView, false, view);
    mat4.rotateY(model, model, .01);
    gl.uniformMatrix4fv(uniformLocations.uModel, false, model);

    // Set the attribute arrays.
    // Note that attributes not used in a shader do not have a bound location
    lgl.bind_attrib_buffer(gl, attributeLocations.aNormal, shape_buffers.normals);
    lgl.bind_attrib_buffer(gl, attributeLocations.aPosition, shape_buffers.positions);

    // And the element array.
    lgl.bind_element_buffer(gl, shape_buffers.cells);

    let count = model3D.cells.length * model3D.cells[0].length;
    gl.drawElements(gl.TRIANGLES, count, gl.UNSIGNED_SHORT, 0);
  }
}

main();
