"use strict";

var fit = require('canvas-fit');
var mat4 = require('gl-mat4');
var normals = require('normals');
var teapot = require('teapot');

var VERTEX_SHADER =
    "attribute vec4 a_position;" +
    "void main() {" +
    "    gl_Position = a_position;" +
    "}";

var FRAGMENT_SHADER =
    "precision mediump float;" +
    "void main() {" +
    "    gl_FragColor = vec4(1, .5, .5, 1);" +
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

function make_buffer(gl, data, mode) {
    // Initialize a buffer.
    var buf = gl.createBuffer();

    // Flatten the data to a packed array.
    var arr = new Float32Array([].concat.apply([], data));

    console.log(arr);

    // Insert the data into the buffer.
    gl.bindBuffer(mode, buf);
    gl.bufferData(mode, arr, gl.STATIC_DRAW);

    return buf;
};

// Given a mesh, with the fields `positions` and `cells`, create three buffers
// for drawing the thing. Return an object with the fields:
// - `cells`, a 3-dimensional uint16 element array buffer
// - `positions`, a 3-dimensional float32 array buffer
// - `normals`, ditto
function mesh_buffers(gl, obj) {
    var norm = normals.vertexNormals(bunny.cells, bunny.positions);

    return {
        cells: make_buffer(gl, obj.cells, 'uint16', gl.ELEMENT_ARRAY_BUFFER),
        positions: make_buffer(gl, obj.positions, 'float32', gl.ARRAY_BUFFER),
        normals: make_buffer(gl, norm, 'float32', gl.ARRAY_BUFFER),
    }
};

function main() {
    var canvas = document.getElementById("c");
    var gl = canvas.getContext("webgl");
    if (!gl) {
        return;
    }

    var vertexShader = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    var fragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, FRAGMENT_SHADER);

    // Link the two shaders into a program
    var program = createProgram(gl, vertexShader, fragmentShader);

    // look up where the vertex data needs to go.
    var positionAttributeLocation = gl.getAttribLocation(program, "a_position");

    var positions = [
        [0, 0],
        [0, 0.5],
        [0.7, 0],
    ];
    var positionBuffer = make_buffer(gl, positions, gl.ARRAY_BUFFER);

    var width = gl.drawingBufferWidth;
    var height = gl.drawingBufferHeight;

    // Tell WebGL how to convert from clip space to pixels
    gl.viewport(0, 0, width, height);

    // Clear the canvas
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);

    // Tell it to use our program (pair of shaders)
    gl.useProgram(program);

    // Turn on the attribute
    gl.enableVertexAttribArray(positionAttributeLocation);

    // Bind the position buffer.
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);

    // Tell the attribute how to get data out of positionBuffer (ARRAY_BUFFER)
    var size = 2;          // 2 components per iteration
    var type = gl.FLOAT;   // the data is 32bit floats
    var normalize = false; // don't normalize the data
    var stride = 0;        // 0 = move forward size * sizeof(type) each iteration to get the next position
    var offset = 0;        // start at the beginning of the buffer
    gl.vertexAttribPointer(positionAttributeLocation, size, type, normalize, stride, offset);

    var count = 3;
    gl.drawArrays(gl.TRIANGLES, 0, count);
}

main();
