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
  // let caiman = fs.readFileSync(__dirname + './../resources/OBJ/caiman.obj', 'utf8');

  // Uniform and attribute locations.
  let loc_uProjection = lgl.uniformLoc(gl, program, 'uProjection');
  let loc_uView = lgl.uniformLoc(gl, program, 'uView');
  let loc_uModel = lgl.uniformLoc(gl, program, 'uModel');
  let loc_uLight = lgl.uniformLoc(gl, program, 'uLight');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');

  // Whether or not to use the reference (correct) phong model
  let loc_uRef = lgl.uniformLoc(gl, program, 'uRef');

  // We'll draw a teapot.
  let mesh = lgl.getBunny(gl);
  // let mesh = lgl.load_obj (gl, caiman);

  // Initialize the model position.
  let model = mat4.create();
  let ref_model = mat4.create();

  // Position the light source for the lighting effect.
  let light = vec3.fromValues(-20., -40., 20.);

  // Set up user movement commands
  let rotateFlag = true;
  let refFlag = false;
  let forward = vec3.fromValues(.3, 0., 0.);
  let backward = vec3.create();
  vec3.negate(backward, forward);
  document.onkeypress = function (evt) {
    evt = evt || window.event;
    if (evt.code == "Space") {
      model = mat4.create();
      rotateFlag = !rotateFlag;
    }
    if (evt.code == "KeyR")
      refFlag = !refFlag
    if (!rotateFlag) {
      if (evt.code == "KeyW")
        mat4.translate(model, model, forward);
      if (evt.code == "KeyS")
        mat4.translate(model, model, backward);
      if (evt.code == "KeyA")
        mat4.rotateY(model, model, .1);
      if (evt.code == "KeyD")
        mat4.rotateY(model, model, -.1);
    }
  }
  mat4.translate(model, model, [0., -10.5, 17.]);

  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.
    // if (rotateFlag)
      // mat4.rotateY(model, model, .01);

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

    gl.disable(gl.CULL_FACE);
    gl.uniform1f(loc_uRef, 0.); 

    // Draw the object.
    lgl.drawMesh(gl, mesh);

    if (refFlag) {
      gl.uniform1f(loc_uRef, 1.); 
      mat4.translate(ref_model, model, [0., 2., 0.]);
      gl.uniformMatrix4fv(loc_uModel, false, ref_model);
      lgl.drawMesh(gl, mesh);
    }
  }
}

main();
