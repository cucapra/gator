/**
 * This module contains common functionality shared across Linguine's WebGL
 * examples.
 */
import { mat4, vec3 } from 'gl-matrix';
import * as model3D from 'teapot';
import * as normals from 'normals';
import pack from 'array-pack-2d';
import canvasOrbitCamera from 'canvas-orbit-camera';

/**
 * Compile a single GLSL shader source file.
 */
export function compileShader(gl: WebGLRenderingContext, shaderType: number, shaderSource: string): WebGLShader {
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

/**
 * Link two compiled shaders (a vertex shader and a fragment shader) together
 * to create a *shader program*, which can be used to issue a draw call.
 */
export function createProgram(gl: WebGLRenderingContext, vertexShader: WebGLShader, fragmentShader: WebGLShader): WebGLProgram {
  // create a program.
  let program = gl.createProgram();
  if (!program) {
    throw "could not create new program";
  }

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

  // Delete shader objects after linked to program.
  gl.deleteShader(vertexShader);
  gl.deleteShader(fragmentShader);

  return program;
}

/**
 * Compile and link a vertex/fragment shader pair.
 */
export function compileProgram(gl: WebGLRenderingContext, vtx: string, frag: string): WebGLProgram {
  let vertexShader = compileShader(gl, gl.VERTEX_SHADER, vtx);
  let fragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, frag);
  return createProgram(gl, vertexShader, fragmentShader);
}

/**
 * Compute a projection matrix (placed in the `out` matrix allocation) given
 * the width and height of a viewport.
 */
export function projection_matrix(out: mat4, width: number, height: number) {
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
}

/**
 * Make a WebGL buffer from a nested "array of arrays" representing a series
 * of short vectors.
 */
function make_buffer(gl: WebGLRenderingContext, data: number[][], type: 'uint8' | 'uint16' | 'float32', mode: number): WebGLBuffer {
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
}

/**
 * Bind a buffer as an attribute array.
 */
export function bind_attrib_buffer(gl: WebGLRenderingContext, location: number, buffer: WebGLBuffer) {
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.vertexAttribPointer(location, 3, gl.FLOAT, false, 0, 0);
  gl.enableVertexAttribArray(location);
}

/**
 * Bind a buffer as an elment array.
 */
export function bind_element_buffer(gl: WebGLRenderingContext, buffer: WebGLBuffer) {
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, buffer);
}

/**
 * Given a mesh, with the fields `positions` and `cells`, create three buffers
 * for drawing the thing. Return an object with the fields:
 * - `cells`, a 3-dimensional uint16 element array buffer
 * - `cell_count`, the total number of numbers in the cell buffer
 * - `positions`, a 3-dimensional float32 array buffer
 * - `normals`, ditto
 */
export function mesh_buffers(gl: WebGLRenderingContext, obj: { cells: [number, number, number][], positions: [number, number, number][] }) {
  let norm = normals.vertexNormals(obj.cells, obj.positions);

  return {
    cells: make_buffer(gl, obj.cells, 'uint16', gl.ELEMENT_ARRAY_BUFFER),
    cell_count: obj.cells.length * obj.cells[0].length,
    positions: make_buffer(gl, obj.positions, 'float32', gl.ARRAY_BUFFER),
    normals: make_buffer(gl, norm, 'float32', gl.ARRAY_BUFFER),
  };
}

/**
 * Get the WebGL rendering context for a <canvas> element.
 *
 * Thow an error if the browser does not support WebGL. If provided,
 * also attach a rendering function that will be called to paint each
 * frame.
 */
export function glContext(canvas: HTMLCanvasElement, render?: () => void): WebGLRenderingContext {
  let gl = canvas.getContext('webgl');
  if (!gl) {
    throw "WebGL not available";
  }

  // Register the animation function.
  if (render) {
    const r = render;  // Avoid funky JavaScript scoping problems.
    let tick = () => {
      r();
      requestAnimationFrame(tick);  // Call us back on the next frame.
    }
    requestAnimationFrame(tick);  // Kick off the first frame.
  }

  return gl;
}

/**
 * Throw an exception if a value is null. Otherwise, return it unchanged.
 */
export function check_null<T>(v: T | null): T {
  if (v === null) {
    throw "value is null";
  }
  return v;
}

/**
 * Given a canvas element to draw in, set up a WebGL context with a render
 * loop that calls the provided function. Return the WebGL context object.
 *
 * The render function is provided with two transformation matrices: a view
 * matrix and a projection matrix.
 *
 * The canvas gets an interactive "orbit camera" that lets the user
 * interactively manipulate the view.
 */
export function setup(canvas: HTMLCanvasElement, render: (view: mat4, projection: mat4) => void): WebGLRenderingContext {
  // Set up the interactive pan/rotate/zoom camera.
  let camera = canvasOrbitCamera(canvas);

  // Initialize the transformation matrices that are dictated by the camera
  // and the canvas dimensions.
  let projection = mat4.create();
  let view = mat4.create();

  // Get the WebGL rendering context, and set up the render loop.
  let gl = glContext(canvas, () => {
    // Update the camera view.
    camera.view(view);
    camera.tick();

    // Update the projection matrix.
    let width = gl.drawingBufferWidth;
    let height = gl.drawingBufferHeight;
    projection_matrix(projection, width, height);

    // Set the rendering context to fill the canvas.
    gl.viewport(0, 0, width, height);

    // Rendering flags.
    gl.enable(gl.DEPTH_TEST);  // Prevent triangle overlap.
    gl.enable(gl.CULL_FACE);  // Triangles not visible from behind.

    render(view, projection);
  });

  // Clear the canvas.
  gl.clearColor(0, 0, 0, 0);
  gl.clear(gl.COLOR_BUFFER_BIT);

  return gl;
}

/**
 * Look up a uniform location (and assert that it is non-null).
 */
export function uniformLoc(gl: WebGLRenderingContext, program: WebGLProgram, name: string) {
  return check_null(gl.getUniformLocation(program, name));
}

/**
 * Look up an attribute location (and assert that it is non-null).
 */
export function attribLoc(gl: WebGLRenderingContext, program: WebGLProgram, name: string) {
  return check_null(gl.getAttribLocation(program, name));
}
