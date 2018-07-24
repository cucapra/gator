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
  let mesh = lgl.getMesh(gl, model3D);

  // Create the base matrices to be used
  // when rendering the object. Alternatively, can
  // be created using `new Float32Array(16)`
  let model = mat4.create();

  // Position the light source for the lighting effect.
  let light = vec3.fromValues(20., 0., 20.);

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
    lgl.bind_attrib_buffer(gl, attributeLocations.aNormal, mesh.normals);
    lgl.bind_attrib_buffer(gl, attributeLocations.aPosition, mesh.positions);

    // Draw the object.
    lgl.drawMesh(gl, mesh);
  }
}

main();
