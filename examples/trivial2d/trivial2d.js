var VERTEX_SHADER = "precision mediump float;" +
    "attribute vec4 aPosition;" +
    "varying vec4 vPosition;" +
    "uniform vec2 uResolution;" +
    "void main() {" +
    "vPosition = aPosition;" +
    "vec2 zeroToOne = aPosition.xy / uResolution;" +
    "vec2 zeroToTwo = zeroToOne * 2.0;" +
    "vec2 clipSpace = zeroToTwo - 1.0;" +
    "gl_Position = vec4(clipSpace, 0, 1);" +
    "}";
var FRAGMENT_SHADER = "precision mediump float;" +
    "varying vec4 vPosition" +
    "void main() {" +
    "gl_FragColor = vPosition;" +
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
    gl.vertexAttribPointer(location, 2, gl.FLOAT, false, 0, 0);
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
    // look up uniform locations
    var resolutionUniformLocation = gl.getUniformLocation(program, "uResolution");
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
        // Bind the position buffer
        bind_attrib_buffer(gl, positionLocation, positionBuffer);
        // set the resolution
        gl.uniform2f(resolutionUniformLocation, gl.canvas.width, gl.canvas.height);
        var count = 6;
        gl.drawArrays(gl.TRIANGLES, 0, count);
    }
}
// Fill the buffer with the values that define a letter 'F'.
function setGeometry(gl) {
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
        100, 200,
        400, 200,
        100, 300,
        100, 300,
        400, 200,
        400, 300
    ]), gl.STATIC_DRAW);
}
main();
