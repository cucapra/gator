import * as lgl from '../lglexample';
import { mat4 } from 'gl-matrix';

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
  let loc_uLight = lgl.uniformLoc(gl, program, 'uLight');
  let loc_uFlashLight = lgl.uniformLoc(gl, program, 'uFlashLight');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');

  // We'll draw a teapot.
  let mesh = lgl.getTeapot(gl);

  // Initialize the model positions.
  let models: mat4[] = [];
  for (let i = 0; i < 5; i++) {
    models.push(mat4.create());
  }
  mat4.translate(models[0], models[0], [40, 0, -50]);
  mat4.translate(models[1], models[1], [0, -15, -40]);
  mat4.translate(models[2], models[2], [-30, 10, 0]);
  mat4.translate(models[3], models[3], [-50, 10, -30]);
  mat4.translate(models[4], models[4], [30, 30, 0]);

  // Set the light positions
  let light = [60., 0., 5.];
  let flashLight = [0., 0., 1.];

  function render(view: mat4, projection: mat4) {
    gl.clearColor(0., 0., 0., 1.);
    gl.clearDepth(1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    for (let i = 0; i < 5; i++) {
      // Rotate the model a little bit on each frame.
      mat4.rotateY(models[i], models[i], i * .005 - .0125);

      // Use our shader pair.
      gl.useProgram(program);

      // Set the shader "uniform" parameters.
      gl.uniformMatrix4fv(loc_uProjection, false, projection);
      gl.uniformMatrix4fv(loc_uView, false, view);
      gl.uniformMatrix4fv(loc_uModel, false, models[i]);
      gl.uniform3fv(loc_uLight, light);
      gl.uniform3fv(loc_uFlashLight, flashLight);

      // Set the attribute arrays.
      lgl.bind_attrib_buffer(gl, loc_aNormal, mesh.normals, 3);
      lgl.bind_attrib_buffer(gl, loc_aPosition, mesh.positions, 3);
      lgl.drawMesh(gl, mesh);
    }
  }
}

main();
