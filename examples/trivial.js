"use strict";

var fit = require('canvas-fit');
var mat4 = require('gl-mat4');
var teapot = require('teapot');
var normals = require('normals');
var canvasOrbitCamera = require('canvas-orbit-camera');
var glContext = require('gl-context');
var pack = require('array-pack-2d');

var VERTEX_SHADER =
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

var FRAGMENT_SHADER =
  "precision mediump float;" +
  "varying vec3 vNormal;" +
  "void main() {" +
  "gl_FragColor = vec4(1.0, .5, .5, 1.0);" +
  "}";

function compileShader(gl, shaderType, shaderSource) {
  // Create the shader object
  var shader = gl.createShader(shaderType);

  // Set the shader source code.
  gl.shaderSource(shader, shaderSource);

  // Compile the shader
  gl.compileShader(shader);

  // Check if it compiled
  var success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
  if (!success) {
    // Something went wrong during compilation; get the error
    throw "could not compile shader:" + gl.getShaderInfoLog(shader);
  }

  return shader;
}

function createProgram(gl, vertexShader, fragmentShader) {
  // create a program.
  var program = gl.createProgram();

  // attach the shaders.
  gl.attachShader(program, vertexShader);
  gl.attachShader(program, fragmentShader);

  // link the program.
  gl.linkProgram(program);

  // Check if it linked.
  var success = gl.getProgramParameter(program, gl.LINK_STATUS);
  if (!success) {
    // something went wrong with the link
    throw ("program filed to link:" + gl.getProgramInfoLog(program));
  }

  return program;
};

// Compute a project matrix (placed in the `out` matrix allocation) given the
// width and height of a viewport.
function projection_matrix(out, width, height) {
  var aspectRatio = width / height;
  var fieldOfView = Math.PI / 4;
  var near = 0.01;
  var far = 100;

  mat4.perspective(out, fieldOfView, aspectRatio, near, far)
};

function make_buffer(gl, data, type, mode) {
  // Initialize a buffer.
  var buf = gl.createBuffer();

  // Flatten the data to a packed array.
  var arr = pack(data, type);

  // Insert the data into the buffer.
  gl.bindBuffer(mode, buf);
  gl.bufferData(mode, arr, gl.STATIC_DRAW);

  return buf;
};

// Set a buffer as an attribute array.
function bind_attrib_buffer(gl, location, buffer) {
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.vertexAttribPointer(location, 3, gl.FLOAT, false, 0, 0);
  gl.enableVertexAttribArray(location);
}

// Set a buffer as the element array.
function bind_element_buffer(gl, buffer) {
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, buffer);
}

// Given a mesh, with the fields `positions` and `cells`, create three buffers
// for drawing the thing. Return an object with the fields:
// - `cells`, a 3-dimensional uint16 element array buffer
// - `positions`, a 3-dimensional float32 array buffer
// - `normals`, ditto
function mesh_buffers(gl, obj) {
  var norm = normals.vertexNormals(obj.cells, obj.positions);

  return {
    cells: make_buffer(gl, obj.cells, 'uint16', gl.ELEMENT_ARRAY_BUFFER),
    positions: make_buffer(gl, obj.positions, 'float32', gl.ARRAY_BUFFER),
    normals: make_buffer(gl, norm, 'float32', gl.ARRAY_BUFFER),
  }
};

function main() {
  var canvas = document.getElementById('c');
  //window.addEventListener('resize', fit(canvas), false);
  var camera = canvasOrbitCamera(canvas);
  var gl = glContext(canvas, render);
  if (!gl) {
    return;
  }

  var vertexShader = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
  var fragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, FRAGMENT_SHADER);

  // Compile the shader program.
  var program = createProgram(gl, vertexShader, fragmentShader);
  var locations = {
    'uProjection': gl.getUniformLocation(program, 'uProjection'),
    'uView': gl.getUniformLocation(program, 'uView'),
    'uModel': gl.getUniformLocation(program, 'uModel'),
    'aPosition': gl.getAttribLocation(program, 'aPosition'),
    'aNormal': gl.getAttribLocation(program, 'aNormal'),
  };

  // look up where the vertex data needs to go.
  var shape_buffers = mesh_buffers(gl, teapot);

  // Create the base matrices to be used
  // when rendering the object. Alternatively, can
  // be created using `new Float32Array(16)`
  var projection = mat4.create();
  var model = mat4.create();
  var view = mat4.create();

  // Clear the canvas
  gl.clearColor(0, 0, 0, 0);
  gl.clear(gl.COLOR_BUFFER_BIT);

  function render() {

    var width = gl.drawingBufferWidth;
    var height = gl.drawingBufferHeight;

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

    var count = teapot.cells.length * teapot.cells[0].length;
    gl.drawElements(gl.TRIANGLES, count, gl.UNSIGNED_SHORT, 0);
  }
}

main();
