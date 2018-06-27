var VERTEX_SHADER = "precision mediump float;" +
    "attribute vec4 aPosition;" +
    "void main() {" +
    "gl_Position = aPosition;" +
    "}";
var FRAGMENT_SHADER = "precision mediump float;" +
    "void main() {" +
    "gl_FragColor = vec4(1.0, .5, .5, 1.0);" +
    "}";
function compileShader(gl, shaderType, shaderSource) {
    // Create the shader object
    var shader = gl.createShader(shaderType);
    if (!shader) {
        throw "could not create shader";
    }
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
}
;
// Set a buffer as an attribute array.
function bind_attrib_buffer(gl, location, buffer) {
    gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
    gl.vertexAttribPointer(location, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(location);
}
function main() {
    var canvas = document.getElementById('c');
    var gl = canvas.getContext("webgl");
    if (!gl) {
        return;
    }
    var vertexShader = compileShader(gl, gl.VERTEX_SHADER, VERTEX_SHADER);
    var fragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, FRAGMENT_SHADER);
    // Compile the shader program.
    var program = createProgram(gl, vertexShader, fragmentShader);
    var positionLocation = gl.getAttribLocation(program, "aPosition");
    // Clear the canvas
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);
    // Create a buffer to put positions in
    var positionBuffer = gl.createBuffer();
    // Bind it to ARRAY_BUFFER (think of it as ARRAY_BUFFER = positionBuffer)
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    setGeometry(gl);
    render();
    function render() {
        gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
        // Clear the canvas.
        gl.clear(gl.COLOR_BUFFER_BIT);
        // Tell it to use our program (pair of shaders)
        gl.useProgram(program);
        // Turn on the attribute
        gl.enableVertexAttribArray(positionLocation);
        // Bind the position buffer.
        gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
        // Tell the attribute how to get data out of positionBuffer (ARRAY_BUFFER)
        var size = 3; // 3 components per iteration
        var type = gl.FLOAT; // the data is 32bit floats
        var normalize = false; // don't normalize the data
        var stride = 0; // 0 = move forward size * sizeof(type) each iteration to get the next position
        var offset = 0; // start at the beginning of the buffer
        gl.vertexAttribPointer(positionLocation, size, type, normalize, stride, offset);
        var count = 16 * 6;
        gl.drawArrays(gl.TRIANGLES, 0, count);
    }
}
// Fill the buffer with the values that define a letter 'F'.
function setGeometry(gl) {
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
        // left column front
        0, 0, 0,
        30, 0, 0,
        0, 150, 0,
        0, 150, 0,
        30, 0, 0,
        30, 150, 0,
        // top rung front
        30, 0, 0,
        100, 0, 0,
        30, 30, 0,
        30, 30, 0,
        100, 0, 0,
        100, 30, 0,
        // middle rung front
        30, 60, 0,
        67, 60, 0,
        30, 90, 0,
        30, 90, 0,
        67, 60, 0,
        67, 90, 0,
        // left column back
        0, 0, 30,
        30, 0, 30,
        0, 150, 30,
        0, 150, 30,
        30, 0, 30,
        30, 150, 30,
        // top rung back
        30, 0, 30,
        100, 0, 30,
        30, 30, 30,
        30, 30, 30,
        100, 0, 30,
        100, 30, 30,
        // middle rung back
        30, 60, 30,
        67, 60, 30,
        30, 90, 30,
        30, 90, 30,
        67, 60, 30,
        67, 90, 30,
        // top
        0, 0, 0,
        100, 0, 0,
        100, 0, 30,
        0, 0, 0,
        100, 0, 30,
        0, 0, 30,
        // top rung right
        100, 0, 0,
        100, 30, 0,
        100, 30, 30,
        100, 0, 0,
        100, 30, 30,
        100, 0, 30,
        // under top rung
        30, 30, 0,
        30, 30, 30,
        100, 30, 30,
        30, 30, 0,
        100, 30, 30,
        100, 30, 0,
        // between top rung and middle
        30, 30, 0,
        30, 30, 30,
        30, 60, 30,
        30, 30, 0,
        30, 60, 30,
        30, 60, 0,
        // top of middle rung
        30, 60, 0,
        30, 60, 30,
        67, 60, 30,
        30, 60, 0,
        67, 60, 30,
        67, 60, 0,
        // right of middle rung
        67, 60, 0,
        67, 60, 30,
        67, 90, 30,
        67, 60, 0,
        67, 90, 30,
        67, 90, 0,
        // bottom of middle rung.
        30, 90, 0,
        30, 90, 30,
        67, 90, 30,
        30, 90, 0,
        67, 90, 30,
        67, 90, 0,
        // right of bottom
        30, 90, 0,
        30, 90, 30,
        30, 150, 30,
        30, 90, 0,
        30, 150, 30,
        30, 150, 0,
        // bottom
        0, 150, 0,
        0, 150, 30,
        30, 150, 30,
        0, 150, 0,
        30, 150, 30,
        30, 150, 0,
        // left side
        0, 0, 0,
        0, 0, 30,
        0, 150, 30,
        0, 0, 0,
        0, 150, 30,
        0, 150, 0
    ]), gl.STATIC_DRAW);
}
main();
