import * as lgl from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';

var __dirname : string;

function main() {
  let gl = lgl.setup(render);

  // Compile our shaders.
  let program = lgl.compileProgram(gl,
    require('./vertex.lgl'),
    require('./fragment.lgl')
  );

  const fs : any = require('fs');
  let caiman = fs.readFileSync(__dirname + './../resources/OBJ/caiman.obj', 'utf8');

  // Uniform and attribute locations.
  let loc_uProjection = lgl.uniformLoc(gl, program, 'uProjection');
  let loc_uView = lgl.uniformLoc(gl, program, 'uView');
  let loc_uModel = lgl.uniformLoc(gl, program, 'uModel');
  //let loc_uLight = lgl.uniformLoc(gl, program, 'uLight');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');
  let loc_uLight1Pos = lgl.uniformLoc(gl, program, 'uLight.pos');
  let loc_uLight1DiffColor = lgl.uniformLoc(gl, program, 'uLight.diffColor');
  let loc_uLight1SpecColor = lgl.uniformLoc(gl, program, 'uLight.specColor');

  // We'll draw a gator
  let mesh = lgl.getBunny(gl);
  // let mesh = lgl.load_obj (gl, caiman);

  // Initialize the model position.
  let model = mat4.create();

  // Position the light source for the lighting effect.
  let light = vec3.fromValues(20., 0., 20.);
  let diffColor = vec3.fromValues(1., 1., 0.);
  let specColor = vec3.fromValues(1., 1., 1.);

  console.log(loc_uLight1Pos);
  console.log(loc_uLight1DiffColor);
  console.log(loc_uLight1SpecColor);

  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.
    mat4.rotateY(model, model, .01);

    // Use our shader pair.
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, model);
    gl.uniform3fv(loc_uLight1Pos, light);
    gl.uniform3fv(loc_uLight1DiffColor, diffColor);
    gl.uniform3fv(loc_uLight1SpecColor, specColor);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aNormal, mesh.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aPosition, mesh.positions, 3);

    gl.disable(gl.CULL_FACE);

    // Draw the object.
    lgl.drawMesh(gl, mesh);
  }
}

main();
