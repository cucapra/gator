import * as lgl from '../lglexample';
import { check_null } from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';
import * as model3D from 'bunny';

import shaderData from './data.json';

function main() {
  let canvas = document.getElementById('c') as HTMLCanvasElement;
  let gl = lgl.setup(canvas, render);

  let program = lgl.compileProgram(gl, shaderData.vertex, shaderData.fragment);

  let uniformLocations: { [key: string]: WebGLUniformLocation } = {
    'uProjection': check_null(gl.getUniformLocation(program, 'uProjection')),
    'uView': check_null(gl.getUniformLocation(program, 'uView')),
    'uModel': check_null(gl.getUniformLocation(program, 'uModel')),
    'uLight': check_null(gl.getUniformLocation(program, 'uLight')),
  };

  let attributeLocations: { [key: string]: number } = {
    'aPosition': check_null(gl.getAttribLocation(program, 'aPosition')),
    'aNormal': check_null(gl.getAttribLocation(program, 'aNormal')),
  };

  // look up where the vertex data needs to go.
  let shape_buffers = lgl.mesh_buffers(gl, model3D);

  // Create the base matrices to be used
  // when rendering the object. Alternatively, can
  // be created using `new Float32Array(16)`
  let model = mat4.create();
  let light = vec3.create();

  // place the light
  light[0] = 20.;
  light[1] = 0.;
  light[2] = 20.;

  // Clear the canvas
  gl.clearColor(0, 0, 0, 0);
  gl.clear(gl.COLOR_BUFFER_BIT);

  function render(view: mat4, projection: mat4) {
    // Tell it to use our program (pair of shaders)
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(uniformLocations.uProjection, false, projection);
    gl.uniformMatrix4fv(uniformLocations.uView, false, view);
    mat4.rotateY(model, model, .01);
    gl.uniformMatrix4fv(uniformLocations.uModel, false, model);
    gl.uniform3fv(uniformLocations.uLight, light);

    // Set the attribute arrays.
    // Note that attributes not used in a shader do not have a bound location
    lgl.bind_attrib_buffer(gl, attributeLocations.aNormal, shape_buffers.normals);
    lgl.bind_attrib_buffer(gl, attributeLocations.aPosition, shape_buffers.positions);

    // And the element array.
    // TODO What is an element array?
    lgl.bind_element_buffer(gl, shape_buffers.cells);

    let count = model3D.cells.length * model3D.cells[0].length;
    gl.drawElements(gl.TRIANGLES, count, gl.UNSIGNED_SHORT, 0);
  }
}

main();
