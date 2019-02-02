(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define(["require", "exports", "../lglexample", "gl-matrix"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    var lgl = require("../lglexample");
    var gl_matrix_1 = require("gl-matrix");
    function main() {
        var gl = lgl.setup(render);
        // Compile our shaders.
        var program = lgl.compileProgram(gl, require('./vertex.lgl'), require('./fragment.lgl'));
        // Uniform and attribute locations.
        var loc_uProjection = lgl.uniformLoc(gl, program, 'uProjection');
        var loc_uView = lgl.uniformLoc(gl, program, 'uView');
        var loc_uModel = lgl.uniformLoc(gl, program, 'uModel');
        var loc_uLight = lgl.uniformLoc(gl, program, 'uLight');
        var loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
        var loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');
        // We'll draw a teapot.
        var mesh = lgl.getBunny(gl);
        // Initialize the model position.
        var model = gl_matrix_1.mat4.create();
        // Position the light source for the lighting effect.
        var light = gl_matrix_1.vec3.fromValues(20., 0., 20.);
        function render(view, projection) {
            // Rotate the model a little bit on each frame.
            gl_matrix_1.mat4.rotateY(model, model, .01);
            // Use our shader pair.
            gl.useProgram(program);
            // Set the shader "uniform" parameters.
            gl.uniformMatrix4fv(loc_uProjection, false, projection);
            gl.uniformMatrix4fv(loc_uView, false, view);
            gl.uniformMatrix4fv(loc_uModel, false, model);
            gl.uniform3fv(loc_uLight, light);
            // Set the attribute arrays.
            lgl.bind_attrib_buffer(gl, loc_aNormal, mesh.normals, 3);
            lgl.bind_attrib_buffer(gl, loc_aPosition, mesh.positions, 3);
            // Draw the object.
            lgl.drawMesh(gl, mesh);
        }
    }
    main();
});
//# sourceMappingURL=main.js.map