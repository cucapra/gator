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
  let shadowDepthTextureSize = 1024;

  // Compile our shaders.
  let program = lgl.compileProgram(gl,
    require('./vertex.lgl'),
    require('./fragment.lgl')
  );

  let treeprogram = lgl.compileProgram(gl,
    require('./treevert.lgl'),
    require('./treefrag.lgl')
  );

  let shadowmap = lgl.compileProgram(gl,
    require('./shadowvertex.lgl'),
    require('./shadowfragment.lgl')
  );

  // Uniform and attribute locations.
  let loc_uProjection = lgl.uniformLoc(gl, program, 'uProjection');
  let loc_uView = lgl.uniformLoc(gl, program, 'uView');
  let loc_uModel = lgl.uniformLoc(gl, program, 'uModel');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  let loc_uLightView = lgl.uniformLoc(gl, program, "uLightView");
  let loc_uLightProjection = lgl.uniformLoc(gl, program, "uLightProjection");

  // Texture things
  let loc_aTexCoord = lgl.attribLoc(gl, program, 'aTexCoord');
  let loc_uTexture = lgl.uniformLoc(gl, program, 'uTexture');

  let tree_uProjection = lgl.uniformLoc(gl, treeprogram, 'uProjection');
  let tree_uView = lgl.uniformLoc(gl, treeprogram, 'uView');
  let tree_uModel = lgl.uniformLoc(gl, treeprogram, 'uModel');
  let tree_aPosition = lgl.attribLoc(gl, treeprogram, 'aPosition');
  let tree_uColor = lgl.uniformLoc(gl, treeprogram, 'uColor');

  let shadowLocations: { [locname: string]: WebGLUniformLocation | number; } = {};
  shadowLocations["uLightProjection"] = lgl.uniformLoc(gl, shadowmap, "uLightProjection");
  shadowLocations["uLightView"] = lgl.uniformLoc(gl, shadowmap, "uLightView");
  shadowLocations["uModel"] = lgl.uniformLoc(gl, shadowmap, "uModel");
  shadowLocations["aPosition"] = lgl.attribLoc(gl, shadowmap, "aPosition");

  // Read in lpshead obj
  // URL must be statically analyzable other than (__dirname) and (__filename)
  // let src = fs.readFileSync(__dirname + './../resources/lpshead/head.obj', 'utf8');
  let groundObj = fs.readFileSync(__dirname + './../resources/outdoor/ground/grass_01.obj', 'utf8');
  let treetrunk = fs.readFileSync(__dirname + './../resources/outdoor/tree/treetrunk.obj', 'utf8');
  let treeleaves = fs.readFileSync(__dirname + './../resources/outdoor/tree/treeleaves.obj', 'utf8');
  // let sky_sphere = fs.readFileSync(__dirname + './../resources/outdoor/sky/globe.obj', 'utf8');

  let ground = lgl.load_obj (gl, groundObj);
  let tree = lgl.load_obj (gl, treetrunk);
  let treel = lgl.load_obj (gl, treeleaves);
  // let plane = lgl.getCube(gl, 100, 100, 1, 1, 1);
  
  // Initialize the model positions.
  let groundModel = mat4.create();
  let treeModel = mat4.create();
  let globeModel = mat4.create();
  // mat4.translate(planeModel, planeModel, [25., -25., 0.]);
  mat4.rotateX(groundModel, groundModel, Math.PI / 2);
  mat4.scale(groundModel, groundModel, [200., 200., 1.]);
  mat4.translate(treeModel, treeModel, [0., 0., 10.]);
  mat4.scale(treeModel, treeModel, [5., 5., 5.]);
  // mat4.scale(globeModel, globeModel, [2000., 2000., 2000.]);

  let textures: WebGLTexture[] = [];
  textures.push(lgl.load_texture(gl, grass) as WebGLTexture);
  textures.push(lgl.load_texture(gl, sky_day) as WebGLTexture);
  console.log(textures);

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
  mat4.ortho(lightProjectionMatrix, -80, 80, -80, 80, -80.0, 160);
  let lightViewMatrix = mat4.create();
  mat4.lookAt(lightViewMatrix, light, [0., 0., 0.], [0., 1., 0.]);

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

    // Use our shader pair.
    gl.useProgram(shadowmap);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(shadowLocations["uLightProjection"], false, lightProjectionMatrix);
    gl.uniformMatrix4fv(shadowLocations["uLightView"], false, lightViewMatrix);

    gl.viewport(0, 0, shadowDepthTextureSize, shadowDepthTextureSize);
    gl.clearColor(0, 0, 0, 1);
    gl.clearDepth(1.0);
    gl.disable(gl.CULL_FACE);

    gl.bindFramebuffer(gl.FRAMEBUFFER, shadowFramebuffer);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    buildShadowBuffers(tree, treeModel);
    buildShadowBuffers(treel, treeModel);
    buildShadowBuffers(ground, groundModel);
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);

    let width = gl.drawingBufferWidth;
    let height = gl.drawingBufferHeight;
    gl.viewport(0, 0, width, height);
    gl.clearColor(0., 0., 0., 0.);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    // Use our shader pair.
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, groundModel);
    gl.uniformMatrix4fv(loc_uLightView, false, lightViewMatrix);
    gl.uniformMatrix4fv(loc_uLightProjection, false, lightProjectionMatrix);
  
    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aPosition, ground.positions, 3);
    lgl.bind_attrib_buffer(gl, loc_aTexCoord, ground.texcoords, 2);
   
    // Draw the object.
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, textures[0]);
    gl.uniform1i(loc_uTexture, 0);
    lgl.drawMesh(gl, ground);

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
