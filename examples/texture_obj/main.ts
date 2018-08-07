import * as lgl from '../lglexample';
import { mat4 } from 'gl-matrix';

const head : string = require('../resources/marble.jpg');
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
  //let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');
  
  // Texture things
  let loc_aTexCoord = lgl.attribLoc(gl, program, 'aTexCoord');
  let loc_uTexture = lgl.uniformLoc(gl, program, 'uTexture');

  // Read in lpshead obj
  let src = fs.readFileSync(__dirname + './../resources/lpshead/head.OBJ', 'utf8');

  let mesh = lgl.load_obj (gl, src);
  
  // Initialize the model position.
  let model = mat4.create();

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
    lgl.bind_attrib_buffer(gl, loc_aTexCoord, mesh.texcoords, 2);
   
    load_texture(gl);

    // Draw the object.
    lgl.drawMesh(gl, mesh);
  }
}

/**
 * Load image texture.
 * [Reference] : https://github.com/cucapra/braid/
 * @param gl rendering context
 */
function load_texture(gl: WebGLRenderingContext) {
    // Create a texture.
    // Asynchronously load an image
    var image = new Image();
    image.src = head;
    var texture = gl.createTexture();
    
    image.addEventListener('load', function() {
      gl.bindTexture(gl.TEXTURE_2D, texture);
      const alignment = 1;
      gl.pixelStorei(gl.UNPACK_ALIGNMENT, alignment);
      // Invert the Y-coordinate. I'm not 100% sure why this is necessary,
      // but it appears to have been invented to convert between the DOM
      // coordinate convention for images and WebGL's convention.
      gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA,
        gl.UNSIGNED_BYTE, image);
      
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
      // clamp to edge gives us non-power-of-2 support
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA,gl.UNSIGNED_BYTE, image);       
      });
}

main();
