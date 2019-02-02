(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define(["require", "exports", "gl-matrix", "teapot", "bunny", "primitive-cube", "icosphere", "normals", "array-pack-2d", "canvas-orbit-camera", "webgl-obj-loader"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    /**
     * This module contains common functionality shared across Linguine's WebGL
     * examples.
     */
    var gl_matrix_1 = require("gl-matrix");
    var teapot = require("teapot");
    var bunny = require("bunny");
    var primitive_cube_1 = require("primitive-cube");
    var icosphere_1 = require("icosphere");
    var normals = require("normals");
    var array_pack_2d_1 = require("array-pack-2d");
    var canvas_orbit_camera_1 = require("canvas-orbit-camera");
    var obj_loader = require("webgl-obj-loader");
    /**
     * Compile a single GLSL shader source file.
     */
    function compileShader(gl, shaderType, shaderSource) {
        // Create the shader object
        var shader = gl.createShader(shaderType);
        if (!shader) {
            throw "could not create shader";
        }
        // Set the shader source code.
        gl.shaderSource(shader, shaderSource);
        console.log(shaderSource);
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
    exports.compileShader = compileShader;
    /**
     * Link two compiled shaders (a vertex shader and a fragment shader) together
     * to create a *shader program*, which can be used to issue a draw call.
     */
    function createProgram(gl, shaders) {
        // create a program.
        var program = gl.createProgram();
        if (!program) {
            throw "could not create new program";
        }
        // attach the shaders.
        shaders.forEach(function (shader) {
            gl.attachShader(program, shader);
        });
        // link the program.
        gl.linkProgram(program);
        // Check if it linked.
        var success = gl.getProgramParameter(program, gl.LINK_STATUS);
        if (!success) {
            // something went wrong with the link
            throw ("program failed to link:" + gl.getProgramInfoLog(program));
        }
        // Delete shader objects after linked to program.
        shaders.forEach(function (shader) {
            gl.deleteShader(shader);
        });
        return program;
    }
    exports.createProgram = createProgram;
    /**
     * Compile and link a vertex/fragment shader pair.
     */
    function compileProgram(gl, vtx, frag) {
        var vertexShader = compileShader(gl, gl.VERTEX_SHADER, vtx);
        var fragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, frag);
        return createProgram(gl, [vertexShader, fragmentShader]);
    }
    exports.compileProgram = compileProgram;
    /**
     * Compile and link a list of shaders
     */
    function compileMultipassProgram(gl, shaders) {
        var toReturn = [];
        shaders.forEach(function (shader) {
            toReturn.push(compileShader(gl, shader.context, shader.shader));
        });
        return createProgram(gl, toReturn);
    }
    exports.compileMultipassProgram = compileMultipassProgram;
    /**
     * Compute a projection matrix (placed in the `out` matrix allocation) given
     * the width and height of a viewport.
     */
    function projection_matrix(out, width, height) {
        // arbitrary constants designed to give a wide field of view
        var aspectRatio = width / height;
        var fieldOfView = Math.PI / 4;
        var near = .1;
        var far = 2000;
        // mat4.perspective(out, fieldOfView, aspectRatio, near, far)
        // Do the above manually for my sanity for now
        var f = 1.0 / Math.tan(fieldOfView / 2), rangeInv = 1.0 / (near - far);
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
    exports.projection_matrix = projection_matrix;
    /**
     * Create and fill a WebGL buffer with a typed array.
     *
     * `mode` should be either `ELEMENT_ARRAY_BUFFER` or `ARRAY_BUFFER`.
     *
     * [Source]: https://github.com/cucapra/braid/
     */
    function gl_buffer(gl, mode, data) {
        var buf = gl.createBuffer();
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
    function make_buffer(gl, data, type, mode) {
        // Initialize a buffer.
        var buf = gl.createBuffer();
        if (!buf) {
            throw "could not create WebGL buffer";
        }
        // Flatten the data to a packed array.
        var arr = array_pack_2d_1.default(data, type);
        // Insert the data into the buffer.
        gl.bindBuffer(mode, buf);
        gl.bufferData(mode, arr, gl.STATIC_DRAW);
        return buf;
    }
    /**
     * Bind a buffer as an attribute array.
     */
    function bind_attrib_buffer(gl, location, buffer, size) {
        gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
        gl.vertexAttribPointer(location, size, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(location);
    }
    exports.bind_attrib_buffer = bind_attrib_buffer;
    /**
     * Bind a buffer as an elment array.
     */
    function bind_element_buffer(gl, buffer) {
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, buffer);
    }
    exports.bind_element_buffer = bind_element_buffer;
    /**
     * Given a mesh, with the fields `positions` and `cells`, create a Mesh object
     * housing the buffers necessary for drawing the thing.
     */
    function getMesh(gl, obj) {
        var norm = normals.vertexNormals(obj.cells, obj.positions);
        return {
            cells: make_buffer(gl, obj.cells, 'uint16', gl.ELEMENT_ARRAY_BUFFER),
            cell_count: obj.cells.length * obj.cells[0].length,
            positions: make_buffer(gl, obj.positions, 'float32', gl.ARRAY_BUFFER),
            normals: make_buffer(gl, norm, 'float32', gl.ARRAY_BUFFER),
            texcoords: make_buffer(gl, norm, 'float32', gl.ARRAY_BUFFER) /* dummy value */
        };
    }
    exports.getMesh = getMesh;
    /**
     * Load a mesh from an OBJ file.
     *
     * [Reference] : https://github.com/cucapra/braid/
     * @param gl      rendering context
     * @param obj_src string literal content of OBJ source file
     */
    function load_obj(gl, obj_src) {
        if (typeof obj_src !== "string") {
            throw "obj source must be a string";
        }
        // // Create a WebGL buffer.
        var mesh = new obj_loader.Mesh(obj_src);
        console.log(mesh.vertices);
        // Match the interface we're using for Mesh objects that come from
        // StackGL.
        var cell = group_array(mesh.indices, 3);
        var position = group_array(mesh.vertices, 3);
        var normal = normals.vertexNormals(cell, position);
        var out = {
            positions: make_buffer(gl, position, 'float32', gl.ARRAY_BUFFER),
            cells: make_buffer(gl, cell, 'uint16', gl.ELEMENT_ARRAY_BUFFER),
            normals: make_buffer(gl, normal, 'float32', gl.ARRAY_BUFFER),
            cell_count: cell.length * cell[0].length,
            // This name I invented -- it's not in the StackGL models.
            texcoords: gl_buffer(gl, gl.ARRAY_BUFFER, new Float32Array(mesh.textures))
        };
        // .obj files can have normals, but if they don't, this parser library
        // (confusingly) fills the array with NaN.
        if (!isNaN(mesh.vertexNormals[0])) {
            out.normals = group_array(mesh.vertexNormals, 3);
        }
        return out;
    }
    exports.load_obj = load_obj;
    /**
     * Load image texture.
     * @param gl rendering context
     */
    function load_texture(gl, img_src) {
        // Create a texture.
        // Asynchronously load an image
        var image = new Image();
        image.src = img_src;
        var texture = gl.createTexture();
        image.addEventListener('load', function () {
            gl.bindTexture(gl.TEXTURE_2D, texture);
            gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, 1);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
            // clamp to edge gives us non-power-of-2 support
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
            gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, image);
        });
    }
    exports.load_texture = load_texture;
    /**
     * Given a flat array, return an array with the elements grouped into
     * sub-arrays of a given size.
     *
     * [Source] : https://github.com/cucapra/braid/
     */
    function group_array(a, size) {
        var out = [];
        for (var i = 0; i < a.length; i += size) {
            out.push(a.slice(i, i + size));
        }
        return out;
    }
    /**
     * Get a Mesh object for a sphere
     */
    function getCube(gl, sx, sy, sz, ny, nz) {
        return getMesh(gl, primitive_cube_1.default(sx, sy, sz, ny, nz));
    }
    exports.getCube = getCube;
    /**
     * Get a Mesh object for a sphere
     */
    function getSphere(gl, subdivisions) {
        return getMesh(gl, icosphere_1.default(subdivisions));
    }
    exports.getSphere = getSphere;
    /**
     * Get a Mesh object for the Stanford bunny.
     */
    function getBunny(gl) {
        return getMesh(gl, bunny);
    }
    exports.getBunny = getBunny;
    /**
     * Get a Mesh object for the Utah teapot.
     */
    function getTeapot(gl) {
        return getMesh(gl, teapot);
    }
    exports.getTeapot = getTeapot;
    /**
     * Use a WebGL `drawElements` call to draw a mesh created by `getMesh` using
     * its elements (cells).
     */
    function drawMesh(gl, mesh) {
        bind_element_buffer(gl, mesh.cells);
        gl.drawElements(gl.TRIANGLES, mesh.cell_count, gl.UNSIGNED_SHORT, 0);
        var errorCode = gl.getError();
        if (errorCode != 0) {
            throw errorCode;
        }
    }
    exports.drawMesh = drawMesh;
    /**
     * Get the WebGL rendering context for a <canvas> element.
     *
     * Thow an error if the browser does not support WebGL. If provided,
     * also attach a rendering function that will be called to paint each
     * frame.
     */
    function glContext(canvas, render) {
        var gl = canvas.getContext('webgl');
        if (!gl) {
            throw "WebGL not available";
        }
        // Register the animation function.
        if (render) {
            registerAnimator(render);
        }
        return gl;
    }
    exports.glContext = glContext;
    /**
     * Register a function to be called to animate every frame.
     *
     * Return a function that can be used to cancel the animation.
     */
    function registerAnimator(func) {
        var rafID;
        var tick = function () {
            func();
            rafID = requestAnimationFrame(tick); // Call us back on the next frame.
        };
        rafID = requestAnimationFrame(tick); // Kick off the first frame.
        return function () {
            cancelAnimationFrame(rafID);
        };
    }
    exports.registerAnimator = registerAnimator;
    /**
     * Throw an exception if a value is null. Otherwise, return it unchanged.
     */
    function check_null(v, s) {
        if (v === null) {
            throw s + " is null";
        }
        return v;
    }
    exports.check_null = check_null;
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
    function setup(render) {
        // Get the first canvas on the document.
        var canvases = document.getElementsByTagName('canvas');
        if (canvases.length === 0) {
            throw "no canvas found";
        }
        var canvas = canvases[0];
        // Set up the interactive pan/rotate/zoom camera.
        var camera = canvas_orbit_camera_1.default(canvas);
        // camera.zoom(-31);
        // Initialize the transformation matrices that are dictated by the camera
        // and the canvas dimensions.
        var projection = gl_matrix_1.mat4.create();
        var view = gl_matrix_1.mat4.create();
        // Get the WebGL rendering context
        var gl = glContext(canvas);
        // Clear the canvas.
        gl.clearColor(0, 0, 0, 0);
        gl.clear(gl.COLOR_BUFFER_BIT);
        // Set up the render loop.
        var cancel = registerAnimator(function () {
            // Update the camera view.
            camera.view(view);
            camera.tick();
            // Update the projection matrix.
            var width = gl.drawingBufferWidth;
            var height = gl.drawingBufferHeight;
            projection_matrix(projection, width, height);
            // Set the rendering context to fill the canvas.
            gl.viewport(0, 0, width, height);
            // Rendering flags.
            gl.enable(gl.DEPTH_TEST); // Prevent triangle overlap.
            gl.enable(gl.CULL_FACE); // Triangles not visible from behind.
            render(view, projection);
        });
        // A **total hack** to cancel previously-registered animation loops.
        var w = window;
        if (w._linguineCancel) {
            w._linguineCancel();
        }
        w._linguineCancel = cancel;
        return gl;
    }
    exports.setup = setup;
    /**
     * Look up a uniform location (and assert that it is non-null).
     */
    function uniformLoc(gl, program, name) {
        return check_null(gl.getUniformLocation(program, name), name);
    }
    exports.uniformLoc = uniformLoc;
    /**
     * Look up an attribute location (and assert that it is non-null).
     */
    function attribLoc(gl, program, name) {
        return check_null(gl.getAttribLocation(program, name), name);
    }
    exports.attribLoc = attribLoc;
});
//# sourceMappingURL=lglexample.js.map