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
  let loc_uLight = lgl.uniformLoc(gl, program, 'uLight');
  let loc_uLightTrans = lgl.uniformLoc(gl, program, 'uLightTrans');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');

  // We'll draw a gator
  // let mesh = lgl.getBunny(gl);
  let mesh = lgl.load_obj (gl, caiman);
  let plane = lgl.getCube(gl, 3, 3, 3, 1, 1);
  // Initialize the model position.
  let model = mat4.create();
  mat4.translate(model, model, [-5., 0., 5.]);

  let lightTrans = mat4.create();

  let planeModel = mat4.create();
  mat4.translate(planeModel, planeModel, [5., 0., 5.]);
  mat4.rotateY(planeModel, planeModel, Math.PI / 4);
  mat4.rotateX(planeModel, planeModel, Math.PI / 4);
  mat4.rotateZ(planeModel, planeModel, Math.PI / 4);

  // Position the light source for the lighting effect.
  let light = vec3.fromValues(0., 0., 5.);

  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.
    mat4.rotateY(model, model, .01);
    mat4.rotateZ(planeModel, planeModel, .01);
    // mat4.rotateZ(lightTrans, lightTrans, .01);

    // Use our shader pair.
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, model);
    gl.uniformMatrix4fv(loc_uLightTrans, false, lightTrans);
    gl.uniform3fv(loc_uLight, light);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aNormal, mesh.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aPosition, mesh.positions, 3);

    gl.disable(gl.CULL_FACE);

    // Draw the object.
    lgl.drawMesh(gl, mesh);
    
    // Set the attribute arrays.
    gl.uniformMatrix4fv(loc_uModel, false, planeModel);
    lgl.bind_attrib_buffer(gl, loc_aNormal, plane.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aPosition, plane.positions, 3);
    lgl.drawMesh(gl, plane);
  }
}

main();
