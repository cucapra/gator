import * as lgl from '../lglexample';
import { mat4 } from 'gl-matrix';

// Texture for LPSHead
const head_lambert : string = require('../resources/lpshead/lambertian.jpg');

// Loads file system implementation in parcel
// * can only call synchronous functions *
const fs : any = require('fs');

var __dirname : string;

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
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  
  // Texture things
  let loc_aTexCoord = lgl.attribLoc(gl, program, 'aTexCoord');
  let loc_uTexture = lgl.uniformLoc(gl, program, 'uTexture');

  // Read in lpshead obj
  // URL must be statically analyzable other than (__dirname) and (__filename)
  let src = fs.readFileSync(__dirname + './../resources/lpshead/head.OBJ', 'utf8');

  let mesh = lgl.load_obj (gl, src);
  
  // Initialize the model position.
  let model = mat4.create();

  // Load image texture
  lgl.load_texture(gl, head_lambert);

  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.
    mat4.rotateY(model, model, .01);

    // Use our shader pair.
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, model);
    
    // Use texture unit 0 for uTexture
    gl.uniform1i(loc_uTexture, 0);
  
    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aPosition, mesh.positions, 3);
    lgl.bind_attrib_buffer(gl, loc_aTexCoord, mesh.texcoords, 2);
   
    // Draw the object.
    lgl.drawMesh(gl, mesh);
  }
}

main();
