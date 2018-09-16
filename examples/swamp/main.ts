import * as lgl from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';

function main() {
  let gl = lgl.setup(render);

  // Compile our shaders.
  let program = lgl.compileProgram(gl,
    require('./vertex.lgl'),
    require('./fragment.lgl')
  );

  // Uniform and attribute locations.
  let loc_uProjection = lgl.uniformLoc(gl, program, 'uProjection');
  let loc_uView = lgl.uniformLoc(gl, program, 'uView');
  let loc_uModel = lgl.uniformLoc(gl, program, 'uModel');
  let loc_uBaseColor = lgl.uniformLoc(gl, program, 'uBaseColor');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');
  let loc_uBrightness = lgl.uniformLoc(gl, program, 'uBrightness');
  let loc_uScale = lgl.uniformLoc(gl, program, 'uScale');
  let loc_uTime = lgl.uniformLoc(gl, program, 'uTime');

  // We'll draw a teapot.
  let mesh = lgl.getTeapot(gl);

  // Initialize the model position.
  let model = mat4.create();
  
  

  function render(view: mat4, projection: mat4) {
    // Use our shader pair.
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, model);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aNormal, mesh.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aPosition, mesh.positions, 3);

    // Set constant values
    gl.uniform3fv(loc_uBaseColor, [0.8156862745098039, 0.25098039215686274, 0.25098039215686274]);
    gl.uniform1f(loc_uBrightness, 2.5);
    gl.uniform1f(loc_uScale, 31.4159265);
    gl.uniform1f(loc_uTime, window.performance.now());

    // Draw the object.
    lgl.drawMesh(gl, mesh);
  }
}

main();
