/**
 * This module contains common functionality shared across Linguine's WebGL
 * examples.
 */
import { mat4, vec3 } from 'gl-matrix';
import * as teapot from 'teapot';
import * as bunny from 'bunny';
import cube from 'primitive-cube';
import icosphere from 'icosphere';
import * as normals from 'normals';
import pack from 'array-pack-2d';
import canvasOrbitCamera from 'canvas-orbit-camera';
import * as obj_loader from 'webgl-obj-loader';

export type Vec3Array = [number, number, number][];

let app = {thing: mat4.create()}

app = {thing : mat4.create()}

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
export function createProgram(gl: WebGLRenderingContext, shaders: WebGLShader[]): WebGLProgram {
  // create a program.
  let program = gl.createProgram();
  if (!program) {
    throw "could not create new program";
  }

  // attach the shaders.
  shaders.forEach(function (shader: WebGLBuffer) {
    gl.attachShader(program!, shader);
  });

  // link the program.
  gl.linkProgram(program);

  // Check if it linked.
  let success = gl.getProgramParameter(program, gl.LINK_STATUS);
  if (!success) {
    // something went wrong with the link
    throw ("program failed to link:" + gl.getProgramInfoLog(program));
  }

  // Delete shader objects after linked to program.
  shaders.forEach(function (shader: WebGLBuffer) {
    gl.deleteShader(shader);
  });

  return program;
}

/**
 * Compile and link a vertex/fragment shader pair.
 */
export function compileProgram(gl: WebGLRenderingContext, vtx: string, frag: string): WebGLProgram {
  console.log(vtx)
  console.log(frag)
  let vertexShader = compileShader(gl, gl.VERTEX_SHADER, vtx);
  let fragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, frag);
  return createProgram(gl, [vertexShader, fragmentShader]);
}

/**
 * Compile and link a list of shaders
 */
export function compileMultipassProgram(gl: WebGLRenderingContext, shaders: { shader: string, context: number }[]): WebGLProgram {
  let toReturn: WebGLShader[] = [];
  shaders.forEach(function (shader) {
    toReturn.push(compileShader(gl, shader.context, shader.shader));
  });
  return createProgram(gl, toReturn);
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
  var far = 1000;

  // mat4.perspective(out, fieldOfView, aspectRatio, near, far)
  // Do the above manually for my sanity for now
  var f = 1.0 / Math.tan(fieldOfView / 2),
    rangeInv = 1.0 / (near - far);

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
 * Create and fill a WebGL buffer with a typed array.
 *
 * `mode` should be either `ELEMENT_ARRAY_BUFFER` or `ARRAY_BUFFER`.
 *
 * [Source]: https://github.com/cucapra/braid/
 */
function gl_buffer(gl: WebGLRenderingContext, mode: number, data: Float32Array | Uint16Array) {
  let buf = gl.createBuffer();
  if (!buf) {
    throw "could not create WebGL buffer";
  }
  gl.bindBuffer(mode, buf);
  gl.bufferData(mode, data, gl.STATIC_DRAW);
  return buf;
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
export function bind_attrib_buffer(gl: WebGLRenderingContext, location: number, buffer: WebGLBuffer, size: number) {
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
  gl.vertexAttribPointer(location, size, gl.FLOAT, false, 0, 0);
  gl.enableVertexAttribArray(location);
}

/**
 * Bind a buffer as an elment array.
 */
export function bind_element_buffer(gl: WebGLRenderingContext, buffer: WebGLBuffer) {
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, buffer);
}

/**
 * Contains buffers for a single 3D object model.
 */
export interface Mesh {
  /**
   * A 3-dimensional uint16 element array buffer.
   */
  cells: WebGLBuffer;

  /**
   * The total number of numbers in the cell buffer.
   */
  cell_count: number;

  /**
   * A 3-dimensional float32 array buffer.
   */
  positions: WebGLBuffer;

  /**
   * Also a 3-dimensional float32 array buffer.
   */
  normals: WebGLBuffer;

  /**
   * 2-Dimensional float32 array buffer.
   */
  texcoords: WebGLBuffer;
}

/**
 * Given a mesh, with the fields `positions` and `cells`, create a Mesh object
 * housing the buffers necessary for drawing the thing.
 */
export function getMesh(gl: WebGLRenderingContext, obj: { cells: [number, number, number][], positions: [number, number, number][] }): Mesh {
  let norm = normals.vertexNormals(obj.cells, obj.positions);

  return {
    cells: make_buffer(gl, obj.cells, 'uint16', gl.ELEMENT_ARRAY_BUFFER),
    cell_count: obj.cells.length * obj.cells[0].length,
    positions: make_buffer(gl, obj.positions, 'float32', gl.ARRAY_BUFFER),
    normals: make_buffer(gl, norm, 'float32', gl.ARRAY_BUFFER),
    texcoords: make_buffer(gl, norm, 'float32', gl.ARRAY_BUFFER) /* dummy value */
  };
}

/**
 * Load a mesh from an OBJ file.
 *
 * [Reference] : https://github.com/cucapra/braid/
 * @param gl      rendering context
 * @param obj_src string literal content of OBJ source file
 */
export function load_obj(gl: WebGLRenderingContext, obj_src: string): Mesh {

  if (typeof obj_src !== "string") {
    throw "obj source must be a string";
  }

  // // Create a WebGL buffer.
  let mesh = new obj_loader.Mesh(obj_src);
  // Match the interface we're using for Mesh objects that come from
  // StackGL.
  let cell = group_array(mesh.indices, 3) as Vec3Array;
  let position = group_array(mesh.vertices, 3) as Vec3Array;
  // let normal = normals.vertexNormals(cell, position);
  let normal = group_array(mesh.vertexNormals, 3) as Vec3Array;
  let out: Mesh = {
    positions: make_buffer(gl, position, 'float32', gl.ARRAY_BUFFER),
    cells: make_buffer(gl, cell, 'uint16', gl.ELEMENT_ARRAY_BUFFER),
    normals: make_buffer(gl, normal, 'float32', gl.ARRAY_BUFFER),
    cell_count: cell.length * cell[0].length,
    // This name I invented -- it's not in the StackGL models.
    texcoords: gl_buffer(gl, gl.ARRAY_BUFFER, new Float32Array(mesh.textures))
  };

  // .obj files can have normals, but if they don't, this parser library
  // (confusingly) fills the array with NaN.
  // if (!isNaN(mesh.vertexNormals[0])) {
  //   out.normals = group_array(mesh.vertexNormals, 3) as Vec3Array;
  // }

  return out;
}

export function load_texture(gl: WebGLRenderingContext, img_src: string) {
  return load_texture_number(gl, img_src, gl.TEXTURE0);
}

export function load_texture_number(gl: WebGLRenderingContext, 
  img_src: string, tex_num: number) {
  return load_texture_clamp(gl, img_src, tex_num, gl.CLAMP_TO_EDGE);
}

/**
 * Load image texture.
 * @param gl rendering context
 */
export function load_texture_clamp(gl: WebGLRenderingContext, 
  img_src: string, tex_num: number, clamp: number) {
  // Create a texture.
  // Asynchronously load an image
  var image = new Image();
  image.src = img_src;
  var texture = gl.createTexture();

  image.addEventListener('load', function () {
    gl.activeTexture(tex_num);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, 1);

    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    // clamp to edge gives us non-power-of-2 support
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, clamp);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, clamp);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, image);
  });

  return texture;
}

/**
 * Given a flat array, return an array with the elements grouped into
 * sub-arrays of a given size.
 *
 * [Source] : https://github.com/cucapra/braid/
 */
function group_array<T>(a: T[], size: number) {
  let out: T[][] = [];
  for (let i = 0; i < a.length; i += size) {
    out.push(a.slice(i, i + size));
  }
  return out;
}

/**
 * Get a Mesh object for a sphere
 */
export function getCube(gl: WebGLRenderingContext, sx: number, sy: number, sz: number, ny: number, nz: number) {
  return getMesh(gl, cube(sx, sy, sz, ny, nz));
}

/**
 * Get a Mesh object for a sphere
 */
export function getSphere(gl: WebGLRenderingContext, subdivisions: number) {
  return getMesh(gl, icosphere(subdivisions));
}

/**
 * Get a Mesh object for the Stanford bunny.
 */
export function getBunny(gl: WebGLRenderingContext) {
  return getMesh(gl, bunny);
}

/**
 * Get a Mesh object for the Utah teapot.
 */
export function getTeapot(gl: WebGLRenderingContext) {
  return getMesh(gl, teapot);
}

/**
 * Use a WebGL `drawElements` call to draw a mesh created by `getMesh` using
 * its elements (cells).
 */
export function drawMesh(gl: WebGLRenderingContext, mesh: Mesh) {
  bind_element_buffer(gl, mesh.cells);
  gl.drawElements(gl.TRIANGLES, mesh.cell_count, gl.UNSIGNED_SHORT, 0);
  let errorCode = gl.getError();
  if (errorCode != 0) {
    throw errorCode;
  }
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
    registerAnimator(render);
  }

  return gl;
}

/**
 * Register a function to be called to animate every frame.
 *
 * Return a function that can be used to cancel the animation.
 */
export function registerAnimator(func: () => void): () => void {
  let rafID: number;
  let tick = () => {
    func();
    rafID = requestAnimationFrame(tick);  // Call us back on the next frame.
  }
  rafID = requestAnimationFrame(tick);  // Kick off the first frame.

  return () => {
    cancelAnimationFrame(rafID);
  };
}

/**
 * Throw an exception if a value is null. Otherwise, return it unchanged.
 */
export function check_null<T>(v: T | null, s: string): T {
  if (v === null) {
    throw s + " is null";
  }
  return v;
}

/**
 * Set up a WebGL context for the first canvas on the page with a render
 * loop that calls the provided function. Return the WebGL context object.
 *
 * The render function is provided with two transformation matrices: a view
 * matrix and a projection matrix.
 *
 * The canvas gets an interactive "orbit camera" that lets the user
 * interactively manipulate the view.
 */
export function setup(render: (view: mat4, projection: mat4) => void): WebGLRenderingContext {
  // Get the first canvas on the document.
  let canvases = document.getElementsByTagName('canvas');
  if (canvases.length === 0) {
    throw "no canvas found";
  }
  let canvas = canvases[0] as HTMLCanvasElement;

  // Set up the interactive pan/rotate/zoom camera.
  let camera = canvasOrbitCamera(canvas);
  // Initialize the transformation matrices that are dictated by the camera
  // and the canvas dimensions.
  let projection = mat4.create();
  let view = mat4.create();

  // Get the WebGL rendering context
  let gl = glContext(canvas);

  // Clear the canvas.
  gl.clearColor(0, 0, 0, 0);
  gl.clear(gl.COLOR_BUFFER_BIT);

  // Set up the render loop.
  let cancel = registerAnimator(() => {
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

  // A **total hack** to cancel previously-registered animation loops.
  let w = window as any;
  if (w._linguineCancel) {
    w._linguineCancel();
  }
  w._linguineCancel = cancel;

  return gl;
}

/**
 * Look up a uniform location (and assert that it is non-null).
 */
export function uniformLoc(gl: WebGLRenderingContext, program: WebGLProgram, name: string) {
  return check_null(gl.getUniformLocation(program, name), name);
}

/**
 * Look up an attribute location (and assert that it is non-null).
 */
export function attribLoc(gl: WebGLRenderingContext, program: WebGLProgram, name: string) {
  return check_null(gl.getAttribLocation(program, name), name);
}

class MovingCube {
  translation: vec3;
  localRotationAxis: vec3;
  localAngularVelocity: number;
  globalRotationAxis: vec3;
  globalAngularVelocity: number;
  color: vec3;

  constructor(i: number){
    this.translation = vec3.fromValues(Math.random()-.5,Math.random()-.5,Math.random()-.5);
    this.localRotationAxis = vec3.fromValues(Math.random(),Math.random(),Math.random());
    this.localAngularVelocity = 0.005 + 0.1*Math.random();
    this.globalRotationAxis = vec3.fromValues(Math.random(),Math.random(),Math.random());
    this.globalAngularVelocity = 0.005 + 0.02*Math.random();
    this.color = vec3.fromValues(Math.random(),Math.random(),Math.random());
  }
}


let movingCubeData: MovingCube[] = [];
for (var i = 0; i <= 3; i++) {
  movingCubeData.push(new MovingCube(i));
  vec3.normalize(movingCubeData[i].localRotationAxis, movingCubeData[i].localRotationAxis);
  vec3.normalize(movingCubeData[i].globalRotationAxis, movingCubeData[i].globalRotationAxis);
  vec3.normalize(movingCubeData[i].translation, movingCubeData[i].translation);
  vec3.scale(movingCubeData[i].translation, movingCubeData[i].translation, 30);
  if (Math.random() < 0.5) {
    movingCubeData[i].globalAngularVelocity *= -1;
  }
}
export function drawCubes(gl:WebGLRenderingContext, model:mat4, frameNumber:number, loc_uModelOBJ:WebGLUniformLocation, cubeMesh:Mesh   ){

  for (var i = 0; i < movingCubeData.length; i++) {  // draw the cubes
      var cd = movingCubeData[i];
      let cdModel = mat4.create();
      mat4.rotate(cdModel, model, frameNumber*cd.globalAngularVelocity, cd.globalRotationAxis);
      mat4.translate(cdModel, cdModel, cd.translation);
      mat4.rotate(cdModel, cdModel, frameNumber*cd.localAngularVelocity, cd.localRotationAxis);

      gl.uniformMatrix4fv(loc_uModelOBJ, false, cdModel);
      // gl.uniform3fv(loc_uColorOBJ, cd.color);

      // Draw the cube.
      drawMesh(gl, cubeMesh);
  }
}

export function loadSkyboxImages(gl:WebGLRenderingContext, urls:string[], skyboxCubemap:WebGLTexture | null){
  var img = new Array(6);
  var ct = 0;

  for (var i = 0; i < 6; i++) {
    img[i] = new Image();
    img[i].onload = function() {
      ct++;
      if (ct == 6) {
        gl.bindTexture(gl.TEXTURE_CUBE_MAP, skyboxCubemap);
        var targets = [
          gl.TEXTURE_CUBE_MAP_POSITIVE_X, gl.TEXTURE_CUBE_MAP_NEGATIVE_X,
          gl.TEXTURE_CUBE_MAP_POSITIVE_Y, gl.TEXTURE_CUBE_MAP_NEGATIVE_Y,
          gl.TEXTURE_CUBE_MAP_POSITIVE_Z, gl.TEXTURE_CUBE_MAP_NEGATIVE_Z
        ];
        for (var j = 0; j < 6; j++) {
          gl.texImage2D(targets[j], 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, img[j]);
          gl.texParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
          gl.texParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        }
        gl.generateMipmap(gl.TEXTURE_CUBE_MAP);
      }
    }
    img[i].src = urls[i];
  }
}

export function getCanvas(){
  return document.getElementsByTagName('canvas')[0] as HTMLCanvasElement;
}