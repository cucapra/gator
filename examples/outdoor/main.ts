import * as lgl from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';

// Texture for LPSHead
const head_lambert : string = require('../resources/lpshead/lambertian.jpg');
const grass : string = require('../resources/outdoor/ground/grass_01.jpg');
const sky_day : string = require('../resources/outdoor/sky/sky_day.jpg');

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

  let treeprogram = lgl.compileProgram(gl,
    require('./treevert.lgl'),
    require('./treefrag.lgl')
  );

  // Uniform and attribute locations.
  let loc_uProjection = lgl.uniformLoc(gl, program, 'uProjection');
  let loc_uView = lgl.uniformLoc(gl, program, 'uView');
  let loc_uModel = lgl.uniformLoc(gl, program, 'uModel');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  
  // Texture things
  let loc_aTexCoord = lgl.attribLoc(gl, program, 'aTexCoord');
  let loc_uTexture = lgl.uniformLoc(gl, program, 'uTexture');

  let tree_uProjection = lgl.uniformLoc(gl, treeprogram, 'uProjection');
  let tree_uView = lgl.uniformLoc(gl, treeprogram, 'uView');
  let tree_uModel = lgl.uniformLoc(gl, treeprogram, 'uModel');
  let tree_aPosition = lgl.attribLoc(gl, treeprogram, 'aPosition');
  let tree_uColor = lgl.uniformLoc(gl, treeprogram, 'uColor');

  // Read in lpshead obj
  // URL must be statically analyzable other than (__dirname) and (__filename)
  // let src = fs.readFileSync(__dirname + './../resources/lpshead/head.obj', 'utf8');
  let ground = fs.readFileSync(__dirname + './../resources/outdoor/ground/grass_01.obj', 'utf8');
  let treetrunk = fs.readFileSync(__dirname + './../resources/outdoor/tree/treetrunk.obj', 'utf8');
  let treeleaves = fs.readFileSync(__dirname + './../resources/outdoor/tree/treeleaves.obj', 'utf8');
  // let sky_sphere = fs.readFileSync(__dirname + './../resources/outdoor/sky/globe.obj', 'utf8');

  let plane = lgl.load_obj (gl, ground);
  let tree = lgl.load_obj (gl, treetrunk);
  let treel = lgl.load_obj (gl, treeleaves);
  // let plane = lgl.getCube(gl, 100, 100, 1, 1, 1);
  
  // Initialize the model positions.
  let planeModel = mat4.create();
  let treeModel = mat4.create();
  let globeModel = mat4.create();
  // mat4.translate(planeModel, planeModel, [25., -25., 0.]);
  mat4.rotateX(planeModel, planeModel, Math.PI / 2);
  mat4.scale(planeModel, planeModel, [200., 200., 1.]);
  mat4.translate(treeModel, treeModel, [0., 0., 10.]);
  mat4.scale(treeModel, treeModel, [5., 5., 5.]);
  // mat4.scale(globeModel, globeModel, [2000., 2000., 2000.]);

  let textures: WebGLTexture[] = [];
  textures.push(lgl.load_texture(gl, grass) as WebGLTexture);
  textures.push(lgl.load_texture(gl, sky_day) as WebGLTexture);
  console.log(textures);

  let trunkColor = vec3.create();
  let leafColor = vec3.create();

  function render(view: mat4, projection: mat4) {
    // Use our shader pair.
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, planeModel);
    
    // Use texture unit 0 for uTexture
  
    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aPosition, plane.positions, 3);
    lgl.bind_attrib_buffer(gl, loc_aTexCoord, plane.texcoords, 2);
   
    // Draw the object.
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, textures[0]);
    gl.uniform1i(loc_uTexture, 0);
    lgl.drawMesh(gl, plane);

    gl.useProgram(treeprogram);
    gl.uniformMatrix4fv(tree_uProjection, false, projection);
    gl.uniformMatrix4fv(tree_uView, false, view);
    gl.uniformMatrix4fv(tree_uModel, false, treeModel);
    gl.uniform3fv(tree_uColor, [.3, .2, .2]);
    lgl.bind_attrib_buffer(gl, tree_aPosition, tree.positions, 3);
    lgl.drawMesh(gl, tree);
    gl.uniform3fv(tree_uColor, [.2, .8, .1]);
    lgl.bind_attrib_buffer(gl, tree_aPosition, treel.positions, 3);
    lgl.drawMesh(gl, treel);
  }
}

main();
