import * as lgl from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';

// Texture for LPSHead
const head_lambert : string = require('../resources/lpshead/lambertian.jpg');
const grassTextureFile : string = require('../resources/outdoor/ground/grass_01.jpg');
const barkTextureFile : string = require('../resources/outdoor/tree/bark2.jpg');
const leafTextureFile : string = require('../resources/outdoor/tree/leaf.png');
// const sky_day : string = require('../resources/outdoor/sky/sky_day.jpg');

// Loads file system implementation in parcel
// * can only call synchronous functions *
const fs : any = require('fs');

var __dirname : string;

function main() {
  let gl = lgl.setup(render);
  let shadowDepthTextureSize = 1024;

  // Compile our shaders.
  let program = lgl.compileProgram(gl,
    require('./vertex.lgl'),
    require('./fragment.lgl')
  );

  let treeprogram = lgl.compileProgram(gl,
    require('./skyvert.lgl'),
    require('./skyfrag.lgl')
  );

  let shadowmap = lgl.compileProgram(gl,
    require('./shadowvertex.lgl'),
    require('./shadowfragment.lgl')
  );

  // Ubershader locations
  let loc_uProjection = lgl.uniformLoc(gl, program, 'uProjection');
  let loc_uView = lgl.uniformLoc(gl, program, 'uView');
  let loc_uModel = lgl.uniformLoc(gl, program, 'uModel');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  let loc_uLightView = lgl.uniformLoc(gl, program, "uLightView");
  let loc_uLightProjection = lgl.uniformLoc(gl, program, "uLightProjection");
  // Texture things
  let loc_aTexCoord = lgl.attribLoc(gl, program, 'aTexCoord');
  let loc_uTexture = lgl.uniformLoc(gl, program, 'uTexture');
  let loc_uShadowTexture = lgl.uniformLoc(gl, program, 'uShadowTexture');

  // Shadow locations
  let shadowLocations: { [locname: string]: WebGLUniformLocation | number; } = {};
  shadowLocations["uLightProjection"] = lgl.uniformLoc(gl, shadowmap, "uLightProjection");
  shadowLocations["uLightView"] = lgl.uniformLoc(gl, shadowmap, "uLightView");
  shadowLocations["uModel"] = lgl.uniformLoc(gl, shadowmap, "uModel");
  shadowLocations["aPosition"] = lgl.attribLoc(gl, shadowmap, "aPosition");

  // Sky locations
  // let skyLocations: { [locname: string]: WebGLUniformLocation | number; } = {};
  // shadowLocations["uProjection"] = lgl.uniformLoc(gl, shadowmap, "uProjection");
  // shadowLocations["uView"] = lgl.uniformLoc(gl, shadowmap, "uView");
  // shadowLocations["uModel"] = lgl.uniformLoc(gl, shadowmap, "uModel");
  // shadowLocations["aPosition"] = lgl.attribLoc(gl, shadowmap, "aPosition");

  // Read in lpshead obj
  // URL must be statically analyzable other than (__dirname) and (__filename)
  // let src = fs.readFileSync(__dirname + './../resources/lpshead/head.obj', 'utf8');
  let groundObj = fs.readFileSync(__dirname + './../resources/outdoor/ground/grass_01.obj', 'utf8');
  let treeObj = fs.readFileSync(__dirname + './../resources/outdoor/tree/treetrunk.obj', 'utf8');
  let treeleavesObj = fs.readFileSync(__dirname + './../resources/outdoor/tree/treeleaves.obj', 'utf8');

  let ground = lgl.load_obj (gl, groundObj);
  let tree = lgl.load_obj (gl, treeObj);
  let treeleaves = lgl.load_obj (gl, treeleavesObj);
  // let sky = lgl.getSphere(gl, 100);
  // let plane = lgl.getCube(gl, 100, 100, 1, 1, 1);
  
  // Initialize the model positions.
  let groundModel = mat4.create();
  let treeModel = mat4.create();
  let skyModel = mat4.create();
  // mat4.translate(planeModel, planeModel, [25., -25., 0.]);
  mat4.translate(groundModel, groundModel, [0., 0., -50.]);
  mat4.rotateX(groundModel, groundModel, - Math.PI / 4);
  mat4.scale(groundModel, groundModel, [100., 100., 1.]);  
  mat4.rotateX(treeModel, treeModel, Math.PI / 4);
  mat4.translate(treeModel, treeModel, [0., -40., 0.]);
  mat4.scale(treeModel, treeModel, [5., 5., 5.]);
  // mat4.scale(globeModel, globeModel, [2000., 2000., 2000.]);

  let grassTexture = lgl.load_texture(gl, grassTextureFile) as WebGLTexture;
  let barkTexture = lgl.load_texture(gl, barkTextureFile) as WebGLTexture;
  let leafTexture = lgl.load_texture(gl, leafTextureFile) as WebGLTexture;

  let light = [-20., 30., 2.];

  let shadowFramebuffer = gl.createFramebuffer()
  gl.bindFramebuffer(gl.FRAMEBUFFER, shadowFramebuffer)

  let shadowDepthTexture = gl.createTexture()
  gl.bindTexture(gl.TEXTURE_2D, shadowDepthTexture)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, shadowDepthTextureSize, shadowDepthTextureSize, 0, gl.RGBA, gl.UNSIGNED_BYTE, null)

  let renderBuffer = gl.createRenderbuffer()
  gl.bindRenderbuffer(gl.RENDERBUFFER, renderBuffer)
  gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, shadowDepthTextureSize, shadowDepthTextureSize)

  gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, shadowDepthTexture, 0)
  gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, renderBuffer)

  gl.bindTexture(gl.TEXTURE_2D, null);
  gl.bindRenderbuffer(gl.RENDERBUFFER, null);

  let lightProjectionMatrix = mat4.create();
  mat4.ortho(lightProjectionMatrix, -160, 160, -160, 160, -80.0, 160);
  let lightViewMatrix = mat4.create();
  mat4.lookAt(lightViewMatrix, light, [0., 0., 0.], [0., 1., 0.]);

  document.onkeypress = function (evt) {
    evt = evt || window.event;
    let charCode = evt.keyCode || evt.which;
    let charStr = String.fromCharCode(charCode);
    if (charStr == "a") {
      light[0] = light[0] + 1.; 
      mat4.lookAt(lightViewMatrix, light, [0., 0., 0.], [0., 1., 0.])
    }
    if (charStr == "d") {
      light[0] = light[0] - 1.; 
      mat4.lookAt(lightViewMatrix, light, [0., 0., 0.], [0., 1., 0.])
    }
  }

  function render(view: mat4, projection: mat4) {

    function buildShadowBuffers(mesh: lgl.Mesh, model: mat4) {
      gl.uniformMatrix4fv(shadowLocations["uModel"], false, model);
      lgl.bind_attrib_buffer(gl, shadowLocations["aPosition"] as number, mesh.positions, 3);
      lgl.bind_element_buffer(gl, mesh.cells);

      gl.drawElements(gl.TRIANGLES, mesh.cell_count, gl.UNSIGNED_SHORT, 0);
      let errorCode = gl.getError();
      if (errorCode != 0) {
        throw errorCode;
      }
    }

    // Set up shadowmap
    gl.useProgram(shadowmap);

    gl.uniformMatrix4fv(shadowLocations["uLightProjection"], false, lightProjectionMatrix);
    gl.uniformMatrix4fv(shadowLocations["uLightView"], false, lightViewMatrix);

    gl.viewport(0, 0, shadowDepthTextureSize, shadowDepthTextureSize);
    gl.clearColor(0, 0, 0, 1);
    gl.clearDepth(1.0);
    gl.disable(gl.CULL_FACE);

    gl.bindFramebuffer(gl.FRAMEBUFFER, shadowFramebuffer);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    buildShadowBuffers(tree, treeModel);
    buildShadowBuffers(treeleaves, treeModel);
    buildShadowBuffers(ground, groundModel);
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);

    // Draw the objects
    gl.useProgram(program);

    gl.activeTexture(gl.TEXTURE1);
    gl.bindTexture(gl.TEXTURE_2D, shadowDepthTexture);

    let width = gl.drawingBufferWidth;
    let height = gl.drawingBufferHeight;
    gl.viewport(0, 0, width, height);
    gl.clearColor(0., 0., 0., 0.);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uLightView, false, lightViewMatrix);
    gl.uniformMatrix4fv(loc_uLightProjection, false, lightProjectionMatrix);
    gl.uniform1i(loc_uTexture, 0);
  
    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aPosition, ground.positions, 3);
    lgl.bind_attrib_buffer(gl, loc_aTexCoord, ground.texcoords, 2);
   
    // Draw the object.
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, grassTexture);
    gl.uniformMatrix4fv(loc_uModel, false, groundModel);
    gl.uniform1i(loc_uShadowTexture, 1);
    lgl.drawMesh(gl, ground);

    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, barkTexture);
    lgl.bind_attrib_buffer(gl, loc_aPosition, tree.positions, 3);
    lgl.bind_attrib_buffer(gl, loc_aTexCoord, tree.texcoords, 2);
    gl.uniformMatrix4fv(loc_uModel, false, treeModel);
    lgl.drawMesh(gl, tree);

    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, leafTexture);
    lgl.bind_attrib_buffer(gl, loc_aPosition, treeleaves.positions, 3);
    lgl.bind_attrib_buffer(gl, loc_aTexCoord, treeleaves.texcoords, 2);
    gl.uniformMatrix4fv(loc_uModel, false, treeModel);
    lgl.drawMesh(gl, treeleaves);
  }
}

main();
