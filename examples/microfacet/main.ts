import * as lgl from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';


// Texture for flower
const flower : string = require('../resources/textures/Flower.png');

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
  // I hate everything about these declarations, btw
  let loc_uProjection = lgl.uniformLoc(gl, program, 'uProjection');
  let loc_uView = lgl.uniformLoc(gl, program, 'uView');
  let loc_uModel = lgl.uniformLoc(gl, program, 'uModel');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');
  // let loc_aTangent = lgl.attribLoc(gl, program, 'aTangent');
  // let loc_aBiTangent = lgl.attribLoc(gl, program, 'aBiTangent');
  let loc_aTexCoord = lgl.attribLoc(gl, program, 'aTexCoord');
  let loc_mat_diffuseColor = lgl.uniformLoc(gl, program, 'mat_diffuseColor');
  let loc_mat_hasDiffuseTexture = lgl.uniformLoc(gl, program, 'mat_hasDiffuseTexture');
  let loc_mat_diffuseTexture = lgl.uniformLoc(gl, program, 'mat_diffuseTexture');
  let loc_mat_indexOfRefraction = lgl.uniformLoc(gl, program, 'mat_indexOfRefraction');
  let loc_mat_alpha = lgl.uniformLoc(gl, program, 'mat_alpha');
  let loc_mat_hasAlphaTexture = lgl.uniformLoc(gl, program, 'mat_hasAlphaTexture');
  let loc_mat_alphaTexture = lgl.uniformLoc(gl, program, 'mat_alphaTexture');
  // let loc_mat_hasNormalTexture = lgl.uniformLoc(gl, program, 'mat_hasNormalTexture');
  // let loc_mat_normalTexture = lgl.uniformLoc(gl, program, 'mat_normalTexture');
  // let loc_mat_normalTextureScale = lgl.uniformLoc(gl, program, 'mat_normalTextureScale');
  let loc_light_eyePosition = lgl.uniformLoc(gl, program, 'light_eyePosition');
  let loc_light_attenuation = lgl.uniformLoc(gl, program, 'light_attenuation');
  let loc_light_color = lgl.uniformLoc(gl, program, 'light_color');

  // Read in lpshead obj
  // URL must be statically analyzable other than (__dirname) and (__filename)
  // let src = fs.readFileSync(__dirname + './../resources/lpshead/head.OBJ', 'utf8');

  // let mesh = lgl.load_obj (gl, src);

  // We'll draw a teapot.
  let mesh = lgl.getTeapot(gl);

  // Initialize the model position.
  let model = mat4.create();
  let light = vec3.create();
  let lightModel = mat4.create();

  // Load image texture
  lgl.load_texture(gl, flower);

  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.
    mat4.rotateY(model, model, .01);

    // Use our shader pair.
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, model);

    gl.uniform3fv(loc_mat_diffuseColor, [1.0,1.0,1.0]);
    gl.uniform1i(loc_mat_diffuseTexture, 0);
    gl.uniform1i(loc_mat_hasDiffuseTexture, 1);
    gl.uniform1i(loc_mat_hasAlphaTexture, 0);
    
    vec3.set(light, 0., 0., 0.);
    // mat4.lookAt(lightModel, [10., 10., 0.], [0., 0., 0.], [0., 1., 0.]);
    vec3.transformMat4(light, light, view);
    gl.uniform3fv(loc_light_eyePosition, light);
    gl.uniform3fv(loc_light_color, [1., 1., 1.]);
    gl.uniform3fv(loc_light_attenuation, [1., 1., 1.]);
    gl.uniform1f(loc_mat_alpha, 0.03);
    gl.uniform1f(loc_mat_indexOfRefraction, 1.33);
    gl.uniform1i(loc_mat_alphaTexture, 0);
    // gl.uniform1i(loc_mat_hasNormalTexture, 0);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aNormal, mesh.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aPosition, mesh.positions, 3);
    // lgl.bind_attrib_buffer(gl, loc_aBiTangent, mesh.positions, 3);
    // lgl.bind_attrib_buffer(gl, loc_aTangent, mesh.positions, 3);
    lgl.bind_attrib_buffer(gl, loc_aTexCoord, mesh.texcoords, 2);

    // Draw the object.
    lgl.drawMesh(gl, mesh);
  }
}

main();
