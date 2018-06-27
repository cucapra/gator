import fit from 'canvas-fit';
import { mat4 } from 'gl-matrix';
import * as teapot from 'teapot';
import * as normals from 'normals';
import * as canvasOrbitCamera from 'canvas-orbit-camera';
import * as glContext from 'gl-context';
import * as pack from 'array-pack-2d';
import data from './color.json'; //not sure?

const VERTEX_SHADER =
  "precision mediump float;" +
  "attribute vec3 aPosition;" +
  "attribute vec3 aNormal;" +
  "varying vec3 vNormal;" +
  "uniform mat4 uProjection;" +
  "uniform mat4 uModel;" +
  "uniform mat4 uView;" +
  "void main() {" +
  "vNormal = aNormal;" +
  "gl_Position = uProjection * uView * uModel * vec4(aPosition, 1.0);" +
  "}";

const FRAGMENT_SHADER = data["main"];

function compileShader(gl: WebGLRenderingContext, shaderType: number, shaderSource: string): WebGLShader {
  // Create the shader object
  let shader = gl.createShader(shaderType);
  if (!shader) {
    throw "could not create shader";
  }

  // Set the shader source code.
  gl.shaderSource(shader, shaderSource);

  // Compile the shader
  gl.compileShader(shader);

  // Check if it compiled
  let success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
  if (!success) {
    // Something went wrong during compilation; get the error
    throw "could not compile shader:" + gl.getShaderInfoLog(shader);
  }

  return shader;
}

function createProgram(gl: WebGLRenderingContext, vertexShader: WebGLShader, fragmentShader: WebGLShader) {
  // create a program.
  let program = gl.createProgram();

  // attach the shaders.
  gl.attachShader(program, vertexShader);
  gl.attachShader(program, fragmentShader);

  // link the program.
  gl.linkProgram(program);

  // Check if it linked.
  let success = gl.getProgramParameter(program, gl.LINK_STATUS);
  if (!success) {
    // something went wrong with the link
    throw ("program filed to link:" + gl.getProgramInfoLog(program));
  }

  return program;
};

// Compute a project matrix (placed in the `out` matrix allocation) given the
// width and height of a viewport.
function projection_matrix(out: any, width: any, height: any) {
  // arbitrary constants designed to give a wide field of view
  var aspectRatio = width / height;
  var fieldOfView = Math.PI / 4;
  var near = .1;
  var far = 2000;

  // mat4.perspective(out, fieldOfView, aspectRatio, near, far)
  // Do the above manually for my sanity for now
  var f = 1.0 / Math.tan(fieldOfView / 2),
    rangeInv = 1.0 / (near - far);

  // doesn't work?
  // out = [
  //   f / aspectRatio, 0, 0, 0,
  //   0, f, 0, 0,
  //   0, 0, (near + far) * rangeInv, -1,
  //   0, 0, (2 * near * far) * rangeInv, 0
  // ];

  out[0] = f / aspectRatio;
  out[1] = 0;
  out[2] = 0;
  out[3] = 0;
  out[4] = 0;
  out[5] = f;
  out[6] = 0;
  out[7] = 0;
  out[8] = 0;
  out[9] = 0;
  out[10] = (far + near) * rangeInv;
  out[11] = -1;
  out[12] = 0;
  out[13] = 0;
  out[14] = (2 * far * near) * rangeInv;
  out[15] = 0;
};

function make_buffer(gl: WebGLRenderingContext, data: number[][], type: string, mode: number): WebGLBuffer {
  // Initialize a buffer.
  let buf = gl.createBuffer();
  if (!buf) {
    throw "could not create WebGL buffer";
  }

  // Flatten the data to a packed array.
  let arr = pack(data, type);

  // Insert the data into the buffer.
  gl.bindBuffer(mode, buf);
  gl.bufferData(mode, arr, gl.STATIC_DRAW);

  return buf;
};

// Set a buffer as an attribute array.
function bind_attrib_buffer(gl: WebGLRenderingContext, location: number, buffer: WebGLBuffer) {
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.vertexAttribPointer(location, 3, gl.FLOAT, false, 0, 0);
  gl.enableVertexAttribArray(location);
}

// Set a buffer as the element array.
function bind_element_buffer(gl: WebGLRenderingContext, buffer: WebGLBuffer) {
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, buffer);
}

// Given a mesh, with the fields `positions` and `cells`, create three buffers
// for drawing the thing. Return an object with the fields:
// - `cells`, a 3-dimensional uint16 element array buffer
// - `positions`, a 3-dimensional float32 array buffer
// - `normals`, ditto
function mesh_buffers(gl: WebGLRenderingContext, obj: { cells: [number, number, number][], positions: [number, number, number][] }) {
  let norm = normals.vertexNormals(obj.cells, obj.positions);

  return {
    cells: make_buffer(gl, obj.cells, 'uint16', gl.ELEMENT_ARRAY_BUFFER),
    positions: make_buffer(gl, obj.positions, 'float32', gl.ARRAY_BUFFER),
    normals: make_buffer(gl, norm, 'float32', gl.ARRAY_BUFFER),
  }
};

function main() {
  let canvas = document.getElementById('c');
  //window.addEventListener('resize', fit(canvas), false);
  let camera = canvasOrbitCamera(canvas);
  let gl = glContext(canvas, render);
  if (!gl) {
    return;
  }

  let vertexShader = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
  let fragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, FRAGMENT_SHADER);

  // Compile the shader program.
  let program = createProgram(gl, vertexShader, fragmentShader);
  let locations = {
    'uProjection': gl.getUniformLocation(program, 'uProjection'),
    'uView': gl.getUniformLocation(program, 'uView'),
    'uModel': gl.getUniformLocation(program, 'uModel'),
    'aPosition': gl.getAttribLocation(program, 'aPosition'),
    'aNormal': gl.getAttribLocation(program, 'aNormal'),
  };

  // look up where the vertex data needs to go.
  let shape_buffers = mesh_buffers(gl, teapot);

  // Create the base matrices to be used
  // when rendering the object. Alternatively, can
  // be created using `new Float32Array(16)`
  let projection = mat4.create();
  let model = mat4.create();
  let view = mat4.create();

  // Clear the canvas
  gl.clearColor(0, 0, 0, 0);
  gl.clear(gl.COLOR_BUFFER_BIT);

  function render() {

    let width = gl.drawingBufferWidth;
    let height = gl.drawingBufferHeight;

    camera.view(view);
    camera.tick();

    projection_matrix(projection, width, height);

    // Set the model to fill the canvas
    gl.viewport(0, 0, width, height);

    // Rendering flags.
    gl.enable(gl.DEPTH_TEST);  // Prevent triangle overlap.
    gl.enable(gl.CULL_FACE);  // Triangles not visible from behind.s

    // Tell it to use our program (pair of shaders)
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(locations.uProjection, false, projection);
    gl.uniformMatrix4fv(locations.uView, false, view);
    gl.uniformMatrix4fv(locations.uModel, false, model);

    // Set the attribute arrays.
    // Note that attributes not used in a shader do not have a bound location
    // bind_attrib_buffer(gl, locations.aNormal, shape_buffers.normals);
    bind_attrib_buffer(gl, locations.aPosition, shape_buffers.positions);

    // And the element array.
    // TODO What is an element array?
    bind_element_buffer(gl, shape_buffers.cells);

    let count = teapot.cells.length * teapot.cells[0].length;
    gl.drawElements(gl.TRIANGLES, count, gl.UNSIGNED_SHORT, 0);
  }
}

main();
