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

  // Initialize the model position.
  let model = mat4.create();
  lgl.addType("model");
  lgl.addType("world");
  lgl.addType("camera");
  lgl.addType("projective");
  lgl.addType("light");
  let lightTrans = mat4.create();
  let modelLightTrans = mat4.create();

  // Position the light source for the lighting effect.
  let light = vec3.fromValues(20., 0., 20.);

  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.
    // mat4.rotateY(model, model, .01);
    lgl.resetGraph();
    
    mat4.rotateZ(lightTrans, lightTrans, .01);
    mat4.rotateZ(modelLightTrans, modelLightTrans, -.01);
    lgl.addMatrixEdge("model", "world", lgl.mat4ToNumArray(model));
    lgl.addMatrixEdge("model", "light", lgl.mat4ToNumArray(modelLightTrans));
    lgl.addMatrixEdge("light", "camera", lgl.mat4ToNumArray(lightTrans));
    lgl.addMatrixEdge("world", "camera", lgl.mat4ToNumArray(view));
    lgl.addMatrixEdge("camera", "projective", lgl.mat4ToNumArray(projection));

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
  }
}

main();
