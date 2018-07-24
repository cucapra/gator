import * as lgl from '../lglexample';
import { check_null } from '../lglexample';
import canvasOrbitCamera from 'canvas-orbit-camera';
import { mat4, vec3 } from 'gl-matrix';
import * as model3D from 'teapot';
import eye from 'eye-vector';

// Import shader source code from our JSON container.
import data from './data.json';
const VERTEX_SHADER = data["vertex"];
const FRAGMENT_SHADER = data["fragment"];

function main() {
  let canvas = document.getElementById('c') as HTMLCanvasElement;
  //window.addEventListener('resize', fit(canvas), false);
  let camera = canvasOrbitCamera(canvas);
  let gl = lgl.glContext(canvas, render);

  let vertexShader = lgl.compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
  let fragmentShader = lgl.compileShader(gl, gl.FRAGMENT_SHADER, FRAGMENT_SHADER);


  // Compile the shader program.
  let program = lgl.createProgram(gl, vertexShader, fragmentShader);
  let uniformLocations: { [key: string]: WebGLUniformLocation } = {
    'uProjection': check_null(gl.getUniformLocation(program, 'uProjection')),
    'uView': check_null(gl.getUniformLocation(program, 'uView')),
    'uModel': check_null(gl.getUniformLocation(program, 'uModel')),
    // 'uLight': check_null(gl.getUniformLocation(program, 'uLight')),
    // 'uCameraPosition': check_null(gl.getUniformLocation(program, 'uCameraPosition')),
  };

  let attributeLocations: { [key: string]: number } = {
    'aPosition': check_null(gl.getAttribLocation(program, 'aPosition')),
    'aNormal': check_null(gl.getAttribLocation(program, 'aNormal')),
  }

  // look up where the vertex data needs to go.
  let shape_buffers = lgl.mesh_buffers(gl, model3D);

  // Create the base matrices to be used
  // when rendering the object. Alternatively, can
  // be created using `new Float32Array(16)`
  let projection = mat4.create();
  let model = mat4.create();
  let view = mat4.create();
  let light = vec3.create();
  let cameraPosition = vec3.create();

  // place the light
  light[0] = 20.;
  light[1] = 0.;
  light[2] = 20.;

  // Clear the canvas
  gl.clearColor(0, 0, 0, 0);
  gl.clear(gl.COLOR_BUFFER_BIT);

  function render() {
    let width = gl.drawingBufferWidth;
    let height = gl.drawingBufferHeight;

    camera.view(view);
    camera.tick();

    eye(view, cameraPosition);
    lgl.projection_matrix(projection, width, height);

    // Set the model to fill the canvas
    gl.viewport(0, 0, width, height);

    // Rendering flags.
    gl.enable(gl.DEPTH_TEST);  // Prevent triangle overlap.
    gl.enable(gl.CULL_FACE);  // Triangles not visible from behind.s

    // Tell it to use our program (pair of shaders)
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(uniformLocations.uProjection, false, projection);
    gl.uniformMatrix4fv(uniformLocations.uView, false, view);
    mat4.rotateY(model, model, .01);
    gl.uniformMatrix4fv(uniformLocations.uModel, false, model);
    gl.uniform3fv(uniformLocations.uLight, light);
    gl.uniform3fv(uniformLocations.uCameraPosition, cameraPosition);

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
