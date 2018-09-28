import * as lgl from '../lglexample';
import { mat4 } from 'gl-matrix';


// TODO: texture rendering is not quite right
// because there is no texture coordinates provided for the teapot
// model.
// Texture for marble
const marble : string = require('../resources/marble.jpg');

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
  //let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');
  
  // Texture things
  let loc_aTexCoord = lgl.attribLoc(gl, program, 'aTexCoord');
  let loc_uTexture = lgl.uniformLoc(gl, program, 'uTexture');

  // We'll draw a teapot
  let mesh = lgl.getTeapot(gl);

  // Initialize the model position.
  let model = mat4.create();

  lgl.load_texture(gl, marble);

  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.
    mat4.rotateY(model, model, .01);

    // Use our shader pair.
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, model);
    gl.uniform1i(loc_uTexture, 0);
  
    // Set the attribute arrays.
    //lgl.bind_attrib_buffer(gl, loc_aNormal, mesh.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aPosition, mesh.positions, 3);
    lgl.bind_attrib_buffer(gl, loc_aTexCoord, mesh.texcoords, 3);
  
    // Draw the object.
    lgl.drawMesh(gl, mesh);
  }
}

main();
