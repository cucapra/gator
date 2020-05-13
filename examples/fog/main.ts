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
  let loc_diffColor = lgl.uniformLoc(gl, program, 'diffColor');
  let loc_uLightTrans = lgl.uniformLoc(gl, program, 'uLightTrans');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');

  // We'll draw a gator
  // let mesh = lgl.getBunny(gl);
  let mesh = lgl.load_obj (gl, caiman);
  let bunny = lgl.getBunny(gl);
  let plane = lgl.getCube(gl, 20, 20, 1, 1, 1);
  // Initialize the model position.
  let model = mat4.create();
  mat4.translate(model, model, [-5., 0., 0.]);

  let lightTrans = mat4.create();

  let bunnyModel = mat4.create();
  mat4.translate(bunnyModel, bunnyModel, [5., 0., 0.]);
  mat4.rotateY(bunnyModel, bunnyModel, Math.PI / 4);
  mat4.rotateX(bunnyModel, bunnyModel, Math.PI / 4);
  mat4.scale(bunnyModel, bunnyModel, [.5, .5, .5]);

  let planeModel = mat4.create();
  mat4.translate(planeModel, planeModel, [0., 0., -5.]);

  // Position the light source for the lighting effect.
  let light = vec3.fromValues(5., 0., 6.);

  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.
    mat4.rotateY(model, model, .01);
    mat4.rotateZ(bunnyModel, bunnyModel, .01);
    // mat4.rotateZ(lightTrans, lightTrans, .01);

    // Use our shader pair.
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, model);
    gl.uniformMatrix4fv(loc_uLightTrans, false, lightTrans);
    gl.uniform3fv(loc_uLight, light);
    gl.uniform3fv(loc_diffColor, [.4, .2, .6]);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aNormal, mesh.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aPosition, mesh.positions, 3);

    gl.disable(gl.CULL_FACE);

    // Draw the object.
    lgl.drawMesh(gl, mesh);
    
    // Set the attribute arrays.
    gl.uniformMatrix4fv(loc_uModel, false, bunnyModel);
    lgl.bind_attrib_buffer(gl, loc_aNormal, bunny.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aPosition, bunny.positions, 3);
    lgl.drawMesh(gl, bunny);

    // Set the attribute arrays.
    gl.uniformMatrix4fv(loc_uModel, false, planeModel);
    lgl.bind_attrib_buffer(gl, loc_aNormal, plane.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aPosition, plane.positions, 3);
    lgl.drawMesh(gl, plane);
  }
}

main();
