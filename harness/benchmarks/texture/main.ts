import * as lgl from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';

// Texture for LPSHead
const head_lambert: string = require('../resources/lpshead/lambertian.jpg');

// Loads file system implementation in parcel
// * can only call synchronous functions *
const fs: any = require('fs');

var __dirname: string;

function main() {
  let [gl, params] = lgl.setup(render);
  const NUM_OBJECTS = parseInt(params['num_objects'] || "100");
  const SHADER = params['shader'] || 'default';


  const shaders = {
    'default': [require('./default/vertex.lgl'), require('./default/fragment.lgl')],
    'raw': [require('./raw/vertex.glsl'), require('./raw/fragment.glsl')],
  };

  const vertices: string[] = new Array(NUM_OBJECTS).fill(shaders[SHADER][0]);
  const frags: string[] = new Array(NUM_OBJECTS).fill(shaders[SHADER][1]);
  const programs: WebGLProgram[] = [];
  for (let i = 0; i < NUM_OBJECTS; i++) {
    programs.push(lgl.compileProgram(gl, vertices[i], frags[i]));
  }

  let locations: { [locname: string]: WebGLUniformLocation | number; }[] = [];
  programs.forEach((program) => {
    // Uniform and attribute locations.
    let location: { [locname: string]: WebGLUniformLocation | number; } = {}
    location['uProjection'] = lgl.uniformLoc(gl, program, 'uProjection');
    location['uView'] = lgl.uniformLoc(gl, program, 'uView');
    location['uModel'] = lgl.uniformLoc(gl, program, 'uModel');
    location['uTexture'] = lgl.uniformLoc(gl, program, 'uTexture');
    location['aPosition'] = lgl.attribLoc(gl, program, 'aPosition');
    location['aTexCoord'] = lgl.attribLoc(gl, program, 'aTexCoord');
    locations.push(location);
  });

  // Read in lpshead obj
  // URL must be statically analyzable other than (__dirname) and (__filename)
  let src = fs.readFileSync(__dirname + './../resources/lpshead/head.OBJ', 'utf8');

  let mesh = lgl.load_obj(gl, src);
  // We'll draw a teapot.
  let meshes = new Array(NUM_OBJECTS).fill(mesh);

  // Initialize the model position.
  let models: mat4[] = [];
  for (let i = 0; i < NUM_OBJECTS; i++)
    models.push(mat4.create());
  const block = Math.floor(Math.sqrt(NUM_OBJECTS));
  const OFFSET = 1;
  for (let i = 0; i < block; i++) {
    for (let j = 0; j < block; j++) {
      mat4.translate(models[i * block + j], models[i * block + j], [OFFSET * i, OFFSET * j, 0]);
    }
  }

  // Load image texture
  lgl.load_texture(gl, head_lambert);

  function render(view: mat4, projection: mat4) {
    for (let i = 0; i < NUM_OBJECTS; i++) {

      // Rotate the model a little bit on each frame.
      mat4.rotateY(models[i], models[i], .01);

      // Use our shader pair.
      gl.useProgram(programs[i]);

      // Set the shader "uniform" parameters.
      gl.uniformMatrix4fv(locations[i]["uProjection"], false, projection);
      gl.uniformMatrix4fv(locations[i]["uView"], false, view);
      gl.uniformMatrix4fv(locations[i]["uModel"], false, models[i]);

      // Use texture unit 0 for uTexture
      gl.uniform1i(locations[i]["uTexture"], 0);

      // Set the attribute arrays.
      lgl.bind_attrib_buffer(gl, locations[i]["aPosition"] as number, meshes[i].positions, 3);
      lgl.bind_attrib_buffer(gl, locations[i]["aTexCoord"] as number, meshes[i].texcoords, 2);

      // Draw the object.
      lgl.drawMesh(gl, meshes[i]);
    }
  }
}

main();
