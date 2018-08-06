/**
 * [Source code] : https://github.com/cucapra/braid/blob/master/glrt/glrt.ts
 * [Source Author]: https://github.com/sampsyo
 * 
 * The run-time support library for WebGL programs. This includes both
 * functions that the compiler emits calls to and utilities that the
 * programmer can invoke themselves.
 *
 * In ideal world, this wouldn't be coupled with the dingus, which is really
 * intended to be a UI. I'd like to refactor it into a separate `glrt`
 * component with its own `package.json` to indicate that it's core run-time
 * support for SCC programs. This should be possible with the "dependency"
 * feature slated for TypeScript 2.1:
 * https://github.com/Microsoft/TypeScript/issues/3469
 */

declare function require(name: string): any;

const eye = require('eye-vector');
import { vec2, vec3, vec4, mat3, mat4 } from 'gl-matrix';
const angle_normals = require('angle-normals');
import * as obj_loader from 'webgl-obj-loader';

export type Vec3Array = [number, number, number][];
export type Vec2Array = [number, number][];

/**
 * The type of the sample meshes we use.
 */
export interface Mesh {
  positions: Vec3Array;
  cells?: Vec3Array;
  texcoords?: Vec2Array;
  normals?: Vec3Array;
  tangents?: Vec3Array;
}

/**
 * Given a flat array, return an array with the elements grouped into
 * sub-arrays of a given size.
 */
function group_array<T>(a: T[], size: number) {
  let out: T[][] = [];
  for (let i = 0; i < a.length; i += size) {
    out.push(a.slice(i, i + size));
  }
  return out;
}

/**
 * The opposite of `group_array`: flatten an array of arrays to a plain array
 * of the element type.
 */
function flat_array<T>(a: T[][]) {
  let out: T[] = [];
  for (let vec of a) {
    for (let el of vec) {
      out.push(el);
    }
  }
  return out;
}

/**
 * Create and fill a WebGL buffer with a typed array.
 *
 * `mode` should be either `ELEMENT_ARRAY_BUFFER` or `ARRAY_BUFFER`.
 */
function gl_buffer(gl: WebGLRenderingContext, mode: number,
                   data: Float32Array | Uint16Array)
{
  let buf = gl.createBuffer();
  gl.bindBuffer(mode, buf);
  gl.bufferData(mode, data, gl.STATIC_DRAW);
  return buf;
}

/**
 * The kinds of assets we support.
 */
export type Asset = string | HTMLImageElement | ArrayBuffer;

/**
 * Pre-loaded assets, keyed by filename.
 */
export type Assets = { [path: string]: Asset };

/**
 * Get an asset string or throw a Loading exception.
 */
function get_asset(assets: Assets, path: string) {
  let asset = assets[path];
  if (!asset) {
    console.log(`asset not loaded: ${path}`);
    let promise = load_asset(path).then((asset) => {
      console.log(`asset loaded.`);
      assets[path] = asset;
    });
    throw new Loading(promise);
  }
  return asset;
}

/**
 * Simple AJAX wrapper for GET requests.
 */
function ajax(url: string, responseType: "text" | "arraybuffer" | "blob" |
              "document" | "json"): Promise<XMLHttpRequest>
{
  return new Promise((resolve: any, reject: any) => {
    let xhr = new XMLHttpRequest();
    xhr.responseType = responseType;
    xhr.onreadystatechange = function () {
      if (xhr.readyState === XMLHttpRequest.DONE) {
        if (xhr.status === 200) {
          resolve(xhr);
        } else {
          let err = `error loading ${url}: `;
          if (xhr.status === 404) {
            err += `not found`;
          } else {
            err += `status ${xhr.status}`;
          }
          console.error(err);
          reject(err);
        }
      }
    };
    xhr.open("GET", url);
    xhr.send();
  });
}

/**
 * Get data via AJAX as a string.
 */
function ajax_get(url: string): Promise<string> {
  return ajax(url, "text").then((xhr) => xhr.response);
}

/**
 * Get binary data via AJAX>
 */
function ajax_get_binary(url: string): Promise<ArrayBuffer> {
  return ajax(url, "arraybuffer").then((xhr) => xhr.response);
}

/**
 * Image loader with the DOM's `new Image` API.
 */
function image_get(url: string): Promise<HTMLImageElement> {
  return new Promise((resolve: any, reject: any) => {
    let img = new Image();
    img.onload = () => {
      resolve(img);
    };
    img.src = url;
  });
}

/**
 * File extensions to fetch as images.
 */
const IMAGE_EXTENSIONS = ['.jpeg', '.jpg', '.png', '.gif'];

/**
 * File extensions to fetch as binary data.
 */
const BINARY_EXTENSIONS = ['.vtx', '.raw'];

/**
 * Check whether a path has a given extension.
 */
function has_extension(path: string, extensions: string[]): boolean {
  for (let ext of extensions) {
    let pos = path.length - ext.length;
    if (path.indexOf(ext) === pos) {
      return true;
    }
  }
  return false;
}

/**
 * Load an asset from the server.
 */
export function load_asset(path: string, baseurl = "assets/"):
  Promise<Asset>
{
  // Fetch the URL either as an image, binary, or string file.
  let url = baseurl + path;
  if (has_extension(path, IMAGE_EXTENSIONS)) {
    return image_get(url);
  } else if (has_extension(path, BINARY_EXTENSIONS)) {
    return ajax_get_binary(url);
  } else {
    return ajax_get(url);
  }
}

/**
 * An exception indicating that an asset is not ready yet.
 *
 * The exception wraps a promise that resolves when the asset is finished
 * loading.
 */
class Loading {
  constructor(
    public promise: Promise<void>
  ) { }
}

/**
 * Run a function repeatedly to load all the assets it needs.
 *
 * This works by catching the `Loading` exception when it's raised, waiting
 * for the load to complete, and then re-executing the function. The next
 * time around, the asset *should* be loaded and the exception shouldn't
 * happen again. I realize that this is extremely messy, but it's a minimally
 * invasive way to add asynchronous loading to synchronous load calls.
 */
export function load_and_run<T>(func: () => T): Promise<T> {
  let out: T;
  try {
    // Try running the function. Assets may not be loaded yet.
    out = func();

  } catch (e) {
    if (e instanceof Loading) {
      // If we're still loading, try again.
      return e.promise.then(() => load_and_run(func));
    } else {
      // Some other exception.
      throw e;
    }
  }

  // If no exception occurred, everything is loaded and we're ready.
  return Promise.resolve(out);
}

/**
 * Parse the `.vtx.raw` mesh data format converted from the Spire examples.
 *
 * Format notes:
 *
 * > 44 bytes for each vertex.
 * > Position: vec3 (byte 0-11)
 * > Normal: vec3 (byte 12-23)
 * > Tangent: vec3 (byte 24-35)
 * > UV: vec2 (byte 36-43)
 *
 * This current version turns the raw data into inefficient JavaScript arrays,
 * just for uniformity. Eventually, it would be nice to keep this data as
 * binary.
 */
function parse_vtx_raw(buffer: ArrayBuffer): Mesh {
  let offset = 0;
  let array = new Float32Array(buffer);

  // Read `count` floats from the array and advance the offset accordingly.
  // Return an ordinary JavaScript array.
  function read_floats(count: number): number[] {
    let out = array.slice(offset, offset + count);
    offset += count;
    return Array.prototype.slice.call(out);
  }

  // Type-safety helpers for reading vectors of fixed sizes.
  function read_vec3() {
    return read_floats(3) as [number, number, number];
  }
  function read_vec2() {
    return read_floats(2) as [number, number];
  }

  // Read the attributes for each vertex.
  let positions: Vec3Array = [];
  let normals: Vec3Array = [];
  let tangents: Vec3Array = [];
  let texcoords: Vec2Array = [];
  while (offset < array.length) {
    positions.push(read_vec3());
    normals.push(read_vec3());
    tangents.push(read_vec3());
    texcoords.push(read_vec2());
  }
  return {
    positions,
    normals,
    tangents,
    texcoords,
  };
}

/**
 * Access an image asset.
 */
function load_image(assets: Assets, name: string): HTMLImageElement {
  let img = get_asset(assets, name);
  if (img instanceof HTMLImageElement) {
    return img;
  } else {
    throw "non-image used as image";
  }
}

/**
 * Get the 6 face indices of a cube map texture.
 *
 * We use this to map the sequence of image arguments to each of the face
 * components in the WebGL API call.
 */
function cubemapTargets(gl: WebGLRenderingContext) {
  return [
    gl.TEXTURE_CUBE_MAP_POSITIVE_X, gl.TEXTURE_CUBE_MAP_NEGATIVE_X,
    gl.TEXTURE_CUBE_MAP_POSITIVE_Y, gl.TEXTURE_CUBE_MAP_NEGATIVE_Y,
    gl.TEXTURE_CUBE_MAP_POSITIVE_Z, gl.TEXTURE_CUBE_MAP_NEGATIVE_Z
  ];
}

/**
 * Convert an image into a WebGL texture.
 *
 * glTextureType is either gl.TEXTURE_2D or gl.TEXTURE_CUBE_MAP. If no images
 * are provided, we create an empty texture.
 */
function texture(gl: WebGLRenderingContext, imgs: HTMLImageElement[],
  glTextureType: number) {

  let tex = gl.createTexture();
  gl.bindTexture(glTextureType, tex);

  // If imgs is null, we need to create an empty texture.
  if (imgs.length === 0) {
    // The size of the empty texture is 1024. Larger than 1024 might lead to
    // low fps.

    if (glTextureType === gl.TEXTURE_2D) {
      // Create an empty standard 2D texture.
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 1024, 1024, 0, gl.RGBA,
                    gl.UNSIGNED_BYTE, null);
    } else {
      // Create an empty cube texture
      cubemapTargets(gl).forEach((target, idx) => (
        gl.texImage2D(target, 0, gl.RGBA, 1024, 1024, 0, gl.RGBA,
                      gl.UNSIGNED_BYTE, null)
      ));
    }

    gl.texParameteri(glTextureType, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texParameteri(glTextureType, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(glTextureType, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(glTextureType, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);

  // Otherwise, create textures from images.
  } else {
    // Create a normal 2D texture.
    if (glTextureType === gl.TEXTURE_2D) {
      // Invert the Y-coordinate. I'm not 100% sure why this is necessary,
      // but it appears to have been invented to convert between the DOM
      // coordinate convention for images and WebGL's convention.
      gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA,
        gl.UNSIGNED_BYTE, imgs[0]);
    } else {
      // Cube mapping. We do not invert Y-coordinate for cubemap textures.
      gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, false);

      // Map each image to its corresponding face.
      cubemapTargets(gl).forEach((target, idx) => (
        gl.texImage2D(target, 0, gl.RGBA, gl.RGBA,
          gl.UNSIGNED_BYTE, imgs[idx])
      ));
    }

    // Interpolation.
    gl.generateMipmap(glTextureType);
    gl.texParameteri(glTextureType, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    gl.texParameteri(glTextureType, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_NEAREST);

    // "Wrap around" the texture on overrun.
    gl.texParameteri(glTextureType, gl.TEXTURE_WRAP_S, gl.REPEAT);
    gl.texParameteri(glTextureType, gl.TEXTURE_WRAP_T, gl.REPEAT);
  }

  gl.bindTexture(glTextureType, null);  // Unbind.
  return tex;
}

/**
 * Create a framebuffer object and bind a depth render buffer to it.
 *
 * @param gl The WebGL context.
 */
function createFramebuffer(gl: WebGLRenderingContext) {
  let framebuffer = gl.createFramebuffer();
  gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);

  // Bind a depth render buffer to this framebuffer in order to enable the
  // depth test.
  let depthBuffer = gl.createRenderbuffer();
  gl.bindRenderbuffer(gl.RENDERBUFFER, depthBuffer);

  // The depth buffer's size should be the same as the texture's (1024x1024).
  gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, 1024, 1024);
  gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT,
    gl.RENDERBUFFER, depthBuffer);

  gl.bindRenderbuffer(gl.RENDERBUFFER, null);  // Unbind render buffer.
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);  // Unbind framebuffer.

  return framebuffer;
}

/**
 * Bind an empty texture to the framebuffer. If the texture is a cube texture,
 * the target is the index of cubemap target list ([posx, negx, posy, negy,
 * posz, negz]). If the texture is a 2D texture, the target is -1.
 *
 * @param gl A WebGL rendering context.
 * @param fbo The framebuffer object that the texture is bound to.
 * @param tex A 2D texture or cube texture.
 * @param target -1 if 2D texture; index if cube texture.
 */
function framebufferTexture(gl: WebGLRenderingContext,
  fbo: WebGLFramebuffer, tex: WebGLTexture, target: number) {
  let glTextureType = target === -1 ? gl.TEXTURE_2D :
    cubemapTargets(gl)[target];
  gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, glTextureType,
                          tex, 0);
  gl.clearColor(0, 0, 0, 1);
  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
}

/*
 * Compile a GLSL program.
 */
function compile_glsl(gl: WebGLRenderingContext, type: number, src: string) {
  let shader = gl.createShader(type);
  gl.shaderSource(shader, src);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    let errLog = gl.getShaderInfoLog(shader);
    console.error("error: compiling shader:", errLog);
  }
  return shader;
}

/**
 * Get the run-time values to expose to WebGL programs.
 *
 * `gl` is the rendering context. `assets` are the pre-loaded data files.
 * `drawtime` is a callback that is invoked with performance information
 * whenever an object is drawn.
 */
export function runtime(gl: WebGLRenderingContext, assets: Assets,
                        drawtime: (ms: number) => void) {
  return {
    // Compile two shaders and link them together, producing a WebGL shader
    // program. (This is called by generated code.)
    get_shader(vertex_source: string, fragment_source: string) {
      let vert = compile_glsl(gl, gl.VERTEX_SHADER, vertex_source);
      let frag = compile_glsl(gl, gl.FRAGMENT_SHADER, fragment_source);
      let program = gl.createProgram();
      gl.attachShader(program, vert);
      gl.attachShader(program, frag);
      gl.linkProgram(program);
      if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        let errLog = gl.getProgramInfoLog(program);
        console.error("error linking program:", errLog);
      }
      return program;
    },

    // Expose the OpenGL context. (The generated code also uses this.)
    gl,

    // Operations exposed to the language for getting data for meshes as WebGL
    // buffers.
    mesh_indices(obj: Mesh) {
      if (!obj.cells) {
        throw "mesh has no indices";
      }
      let data = flat_array(obj.cells);
      return gl_buffer(gl, gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(data));
    },

    mesh_positions(obj: Mesh) {
      let data = flat_array(obj.positions);
      return gl_buffer(gl, gl.ARRAY_BUFFER, new Float32Array(data));
    },

    mesh_normals(obj: Mesh) {
      // Some mesh formats come with normals. Others need them to be
      // calculated.
      let norm: Vec3Array;
      if (obj.normals) {
        norm = obj.normals;
      } else {
        norm = angle_normals(obj.cells, obj.positions);
      }

      let data = flat_array(norm);
      return gl_buffer(gl, gl.ARRAY_BUFFER, new Float32Array(data));
    },

    mesh_tangents(obj: Mesh) {
      if (!obj.tangents) {
        throw "mesh has no tangents";
      }
      let data = flat_array(obj.tangents);
      return gl_buffer(gl, gl.ARRAY_BUFFER, new Float32Array(data));
    },

    // The size, in scalar numbers, of the index array.
    mesh_size(obj: Mesh) {
      return obj.cells!.length * obj.cells![0].length;
    },

    // The size, in scalar numbers, of the vertex position array.
    mesh_count(obj: Mesh) {
      return obj.positions.length * obj.positions[0].length;
    },

    mesh_texcoords(obj: Mesh) {
      let coords = obj.texcoords;
      if (!coords) {
        throw "mesh does not have texture coordinates";
      }

      // Create a WebGL buffer.
      let data = flat_array(coords);
      return gl_buffer(gl, gl.ARRAY_BUFFER, new Float32Array(data));
    },

    // And, similarly, a function for actually drawing a mesh. This takes the
    // indices buffer for the mesh and its size (in the number of scalars).
    draw_mesh(indices: WebGLBuffer, size: number) {
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indices);
      let start = performance.now();
      gl.drawElements(gl.TRIANGLES, size, gl.UNSIGNED_SHORT, 0);
      let end = performance.now();
      drawtime(end - start);
    },

    // An alternative to `draw_mesh` for using `glDrawArrays`, i.e., without
    // an explicit vertex indices. `size` is the number of primitives to draw
    // (I think).
    draw_arrays(size: number) {
      let start = performance.now();
      gl.drawArrays(gl.TRIANGLES, 0, size / 3);
      let end = performance.now();
      drawtime(end - start);
    },

    /**
     * Create a glArrayBuffer from a two dimensional array.
     */
    array_buffer(array: number[][]) {
      let data = flat_array(array);
      return gl_buffer(gl, gl.ARRAY_BUFFER, new Float32Array(data));
    },

    /**
     * Create a glElementArrayBuffer from an int array
     */
    element_buffer(data: number[]) {
      return gl_buffer(gl, gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(data));
    },

    // Expose the gl-matrix library's components.
    mat4,
    mat3,
    vec4,
    vec3,
    vec2,

    // Eye vector calculation.
    eye,

    // A framebuffer value representing the content to be shown on screen,
    // in the canvas. The value is null so that, when we call
    // gl.bindFrameBuffer(null), we unbind from any other framebuffer and
    // draw to the canvas.
    screenbuffer: null,

    // Load a mesh from an OBJ file.
    load_obj(name: string) {
      let obj_src = get_asset(assets, name);
      if (typeof obj_src !== "string") {
        throw "obj source must be a string";
      }
      let mesh = new obj_loader.Mesh(obj_src);

      // Match the interface we're using for Mesh objects that come from
      // StackGL.
      let out: Mesh = {
        positions: group_array(mesh.vertices, 3) as Vec3Array,
        cells: group_array(mesh.indices, 3) as Vec3Array,

        // This name I invented -- it's not in the StackGL models.
        texcoords: group_array(mesh.textures, 2) as Vec2Array,
      };

      // .obj files can have normals, but if they don't, this parser library
      // (confusingly) fills the array with NaN.
      if (!isNaN(mesh.vertexNormals[0])) {
        out.normals = group_array(mesh.vertexNormals, 3) as Vec3Array;
      }

      return out;
    },

    /**
     * Load an image asset.
     */
    load_image(name: string) {
      return load_image(assets, name);
    },

    /**
     * Convert an image, or several images, to a texture. This can be called
     * either with one image to create a standard 2D texture or six images
     * to create a cube-map texture.
     */
    texture(img: HTMLImageElement) {
      if (arguments.length === 0) {
        return texture(gl, [], gl.TEXTURE_2D);
      } else {
        return texture(gl, [img], gl.TEXTURE_2D);
      }
    },

    cubeTexture(...imgs: HTMLImageElement[]) {
      return texture(gl, imgs, gl.TEXTURE_CUBE_MAP);
    },

    /**
     * Load an image asset as a WebGL texture object.
     */
    load_texture(name: string) {
      let img = load_image(assets, name);
      return texture(gl, [img], gl.TEXTURE_2D);
    },

    /**
     * Create a framebuffer object.
     */
    createFramebuffer() {
      return createFramebuffer(gl);
    },

    /**
     * Bind a texture to a framebuffer object.
     */
    framebufferTexture(fbo: WebGLFramebuffer, tex: WebGLTexture,
      target: number) {
      // texture2D if length === 2
      if (arguments.length === 2) {
        // target = -1 stands for texture2D
        framebufferTexture(gl, fbo, tex, -1);
      } else {
        // bind cubeTexture to the framebuffer
        framebufferTexture(gl, fbo, tex, target);
      }
    },

    /**
     * Set the pipeline destination to a framebuffer or the screenbuffer
     */
    bindFramebuffer(fbo: WebGLFramebuffer) {
      if (fbo === null) {
        gl.bindFramebuffer(gl.FRAMEBUFFER, null);
        gl.viewport(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight);
      } else {
        gl.bindFramebuffer(gl.FRAMEBUFFER, fbo);
        // The size of viewport should be consistent with the size of the
        // framebuffer.
        gl.viewport(0, 0, 1024, 1024);
      }
    },

    /**
     * Load a mesh from a `.vtx.raw` file (from the Spire examples).
     */
    load_raw(name: string) {
      let buffer = get_asset(assets, name);
      if (buffer instanceof ArrayBuffer) {
        return parse_vtx_raw(buffer);
      } else {
        throw "non-binary data used as raw mesh";
      }
    },

    /**
     * Create a buffer of values.
     */
    float_array() {
      let arr = new Float32Array(arguments);

      let buf = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, buf);
      gl.bufferData(gl.ARRAY_BUFFER, arr, gl.STATIC_DRAW);

      return buf;
    },

    /**
     * Get an average color from a texture.
     */
    average(img: HTMLImageElement) {
      // Apparently, this is the only way to access the image data from an
      // <img> element.
      let canvas = document.createElement('canvas');
      canvas.width = img.width;
      canvas.height = img.height;
      let context = canvas.getContext('2d');
      if (!context) {
        console.error("could not get canvas context");
        return;
      }
      context.drawImage(img, 0, 0, img.width, img.height);
      let data = context.getImageData(0, 0, img.width, img.height).data;

      // Sum the pixels.
      let totals = [0, 0, 0, 0];  // Red, green, blue, alpha.
      let offset = 0;
      for (let x = 0; x < img.width; ++x) {
        for (let y = 0; y < img.height; ++y) {
          for (let i = 0; i < 4; ++i) {
            totals[i] += data[offset + i];
          }
          offset += 4;
        }
      }

      // Average.
      let count = img.width * img.height;
      let averages: number[] = [];
      for (let i = 0; i < 4; ++i) {
        averages[i] = totals[i] / 255 / count;
      }
      return averages;
    },

    // Helper functions used by the WebGL compiler backend.
    mat3fromOneValue(x: number) {
      let out = mat3.fromValues(x, x, x,
                                x, x, x,
                                x, x, x);
      return out;
    },

    mat4fromOneValue(x: number) {
      let out = mat4.fromValues(x, x, x, x,
                                x, x, x, x,
                                x, x, x, x,
                                x, x, x, x);
      return out;
    },

    mat3div(a: mat3, b: mat3) {
      let out = mat3.create();
      out[0] = a[0] / b[0];
      out[1] = a[1] / b[1];
      out[2] = a[2] / b[2];
      out[3] = a[3] / b[3];
      out[4] = a[4] / b[4];
      out[5] = a[5] / b[5];
      out[6] = a[6] / b[6];
      out[7] = a[7] / b[7];
      out[8] = a[8] / b[8];
      return out;
    },

    mat4div(a: mat4, b: mat4) {
      let out = mat4.create();
      out[0] = a[0] / b[0];
      out[1] = a[1] / b[1];
      out[2] = a[2] / b[2];
      out[3] = a[3] / b[3];
      out[4] = a[4] / b[4];
      out[5] = a[5] / b[5];
      out[6] = a[6] / b[6];
      out[7] = a[7] / b[7];
      out[8] = a[8] / b[8];
      out[9] = a[9] / b[9];
      out[10] = a[10] / b[10];
      out[11] = a[11] / b[11];
      out[12] = a[12] / b[12];
      out[13] = a[13] / b[13];
      out[14] = a[14] / b[14];
      out[15] = a[15] / b[15];
      return out;
    },

    // Create vec3 using vec4
    vec3fromvec4(v4: vec4) {
      let out = vec3.create();
      out[0] = v4[0] || 0.0;
      out[1] = v4[1] || 0.0;
      out[2] = v4[2] || 0.0;
      return out;
    },

    // Create vec4 using vec3
    vec4fromvec3(v3: vec3, x: number) {
      let out = vec4.create();
      out[0] = v3[0] || 0.0;
      out[1] = v3[1] || 0.0;
      out[2] = v3[2] || 0.0;
      out[3] = x || 0.0;
      return out;
    },

    // normalize a scalar
    // s > 0: s = 1
    // s = 0: s = 0
    // s < 0: s = -1
    normalizeScalar(s: number) {
      if (s > 0) {
        return 1;
      } else if (s < 0) {
        return -1;
      } else {
        return 0;
      }
    }
  };
}

export default runtime;