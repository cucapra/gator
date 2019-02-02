import * as lgl from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';

const PROGRAM_COUNT = 50;

function main() {
  let [gl, cancel] = lgl.setup(render);

  // let vertices: string[] = new Array(PROGRAM_COUNT).fill(require('./vertex.lgl'));
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
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');

  // We'll draw a teapot.
  let mesh = lgl.getBunny(gl);

  // Initialize the model position.
  let model = mat4.create();

  // Position the light source for the lighting effect.
  let light = vec3.fromValues(20., 0., 20.);
  let numFrames = 0;
  let start = new Date().getTime();
  setInterval(() => {
    console.log(numFrames / ((new Date().getTime() - start) / 1000));
    numFrames = 0, start = new Date().getTime();
  }, 1000);
  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.
    numFrames++;
    mat4.rotateY(model, model, .01);

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
