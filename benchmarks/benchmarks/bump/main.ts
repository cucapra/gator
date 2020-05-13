import * as lgl from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';

// Textures
const earthmap1k : string = require('../resources/textures/earthmap1k.jpg');
const earthbump1k : string = require('../resources/textures/earthbump1k.jpg');

// Loads file system implementation in parcel
// * can only call synchronous functions *
const fs : any = require('fs');

function main() {
  let [gl, params] = lgl.setup(render);
  const NUM_OBJECTS = parseInt(params['num_objects'] || "100");
  const SHADER = params['shader'] || 'default';

  const shaders = {
    'default': [require('./default/vertex.lgl'), require('./default/fragment.lgl')],
    'raw': [require('./raw/vertex.glsl'), require('./raw/fragment.glsl')]
  };

  let vert = shaders[SHADER][0];
  let frag = shaders[SHADER][1];

  // Compile our shaders.
  let program = lgl.compileProgram(gl,
    vert, frag
  );
  // Uniform and attribute locations.
  let loc_uProjection = lgl.uniformLoc(gl, program, 'uProjection');
  let loc_uView = lgl.uniformLoc(gl, program, 'uView');
  let loc_uModel = lgl.uniformLoc(gl, program, 'uModel');
  // let loc_uNormal = lgl.uniformLoc(gl, program, 'uNormal');

  // Texture things
  let loc_uTexture = lgl.uniformLoc(gl, program, 'uDiffuseTexture');
  let loc_uBumpMap = lgl.uniformLoc(gl, program, 'uDisplacementMap');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  // let loc_aDerivU = lgl.attribLoc(gl, program, 'aDerivU');
  // let loc_aDerivV = lgl.attribLoc(gl, program, 'aDerivV');
  let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');
  let loc_aUv = lgl.attribLoc(gl, program, 'aUv');

  // Read in lpshead obj
  // URL must be statically analyzable other than (__dirname) and (__filename)
  let src = fs.readFileSync(__dirname + '/../resources/OBJ/sphere_highres.obj', 'utf8');

  let mesh = lgl.load_obj (gl, src);

  // Initialize the model position.
  let model = mat4.create();

  // Load image texture
  lgl.load_texture_number(gl, earthmap1k, gl.TEXTURE0);

  // Load bump map
  lgl.load_texture_number(gl, earthbump1k, gl.TEXTURE1)

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
    gl.uniform1i(loc_uBumpMap, 1);
  
    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aPosition, mesh.positions, 3);
    lgl.bind_attrib_buffer(gl, loc_aNormal, mesh.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aUv, mesh.texcoords, 2);
    // lgl.bind_attrib_buffer(gl, loc_aDerivU, mesh.derivU, 2); // TODO 
   
    // Draw the object.
    lgl.drawMesh(gl, mesh);
  }
}

main();
