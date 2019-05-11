import * as lgl from '../lglexample';
import { mat4, mat3, vec4, vec3, quat } from 'gl-matrix';

// Texture for LPSHead
const head_lambert : string = require('../resources/lpshead/lambertian.jpg');
const grassTextureFile : string = require('../resources/outdoor/sky/groundbig.jpg');
const barkTextureFile : string = require('../resources/outdoor/tree/bark.jpg');
const leafTextureFile : string = require('../resources/outdoor/tree/leaf.png');
const nightTextureFile : string = require('../resources/outdoor/sky/night.jpg');
var urls2 = [
  require('../resources/outdoor/sky/night.jpg'), require('../resources/outdoor/sky/night.jpg'), 
  require('../resources/outdoor/sky/night.jpg'), require('../resources/outdoor/sky/night.jpg'), 
  require('../resources/outdoor/sky/night.jpg'), require('../resources/outdoor/sky/night.jpg')
];
var urls = [
  require('../resources/park/posx.jpg'), require('../resources/park/negx.jpg'), 
  require('../resources/park/posy.jpg'), require('../resources/park/negy.jpg'), 
  require('../resources/park/posz.jpg'), require('../resources/park/negz.jpg')
];

// Loads file system implementation in parcel
// * can only call synchronous functions *
const fs : any = require('fs');

var __dirname : string;

function main() {
  let gl = lgl.setup(render);
  let shadowDepthTextureSize = 2048;

  // Compile our shaders.
  let uberprogram = lgl.compileProgram(gl,
    require('./vertex.lgl'),
    require('./fragment.lgl')
  );

  let shadowmap = lgl.compileProgram(gl,
    require('./shadowvertex.lgl'),
    require('./shadowfragment.lgl')
  );

  let programSB = lgl.compileProgram(gl,
    require('./vertexSB.lgl'),
    require('./fragmentSB.lgl')
  );

  // Ubershader locations
  let loc_uProjection = lgl.uniformLoc(gl, uberprogram, 'uProjection');
  let loc_uView = lgl.uniformLoc(gl, uberprogram, 'uView');
  let loc_uModel = lgl.uniformLoc(gl, uberprogram, 'uModel');
  let loc_aPosition = lgl.attribLoc(gl, uberprogram, 'aPosition');
  let loc_aNormal = lgl.attribLoc(gl, uberprogram, 'aNormal');
  let loc_uLight = lgl.uniformLoc(gl, uberprogram, 'uLight');
  let loc_uTime = lgl.uniformLoc(gl, uberprogram, 'uTime');
  let loc_uSpecStrength = lgl.uniformLoc(gl, uberprogram, 'uSpecStrength');
  let loc_uLightView = lgl.uniformLoc(gl, uberprogram, "uLightView");
  let loc_uLightProjection = lgl.uniformLoc(gl, uberprogram, "uLightProjection");

  // Texture things
  let loc_aTexCoord = lgl.attribLoc(gl, uberprogram, 'aTexCoord');
  let loc_uTexture = lgl.uniformLoc(gl, uberprogram, 'uTexture');
  let loc_uShadowTexture = lgl.uniformLoc(gl, uberprogram, 'uShadowTexture');

  // Shadow locations
  let shadowLocations: { [locname: string]: WebGLUniformLocation | number; } = {};
  shadowLocations["uLightProjection"] = lgl.uniformLoc(gl, shadowmap, "uLightProjection");
  shadowLocations["uLightView"] = lgl.uniformLoc(gl, shadowmap, "uLightView");
  shadowLocations["uModel"] = lgl.uniformLoc(gl, shadowmap, "uModel");
  shadowLocations["aPosition"] = lgl.attribLoc(gl, shadowmap, "aPosition");

  // Skybox locations
  let loc_uProjectionSB = lgl.uniformLoc(gl, programSB, 'uProjection');
  let loc_uViewSB = lgl.uniformLoc(gl, programSB, 'uView');
  let loc_uModelSB = lgl.uniformLoc(gl, programSB, 'uModel');
  let loc_aPositionSB = lgl.attribLoc(gl, programSB, 'aPosition');
  let loc_uTimeSB = lgl.uniformLoc(gl, programSB, 'uTime');
  let loc_uLightSB = lgl.uniformLoc(gl, programSB, 'uLight');
  // let loc_uCIEtoRGB = lgl.uniformLoc(gl, programSB, 'uCietorgb');
  
  // Read in object files
  let groundObj = fs.readFileSync(__dirname + './../resources/outdoor/ground/grass_01.obj', 'utf8');
  let treeObj = fs.readFileSync(__dirname + './../resources/outdoor/tree/treetrunk2.obj', 'utf8');
  let treeleavesObj = fs.readFileSync(__dirname + './../resources/outdoor/tree/treeleaves2.obj', 'utf8');

  let ground = lgl.load_obj(gl, groundObj);
  let tree = lgl.load_obj (gl, treeObj);
  let treeleaves = lgl.load_obj (gl, treeleavesObj);
  let skybox = lgl.getCube(gl, 300, 300, 300, 1, 1);
    
  // Initialize the model positions.
  let skyboxModel = mat4.create();
  let groundModel = mat4.create();
  mat4.scale(groundModel, groundModel, [200., 200., 1.]);  
  let treeScale = 0.;
  let treeModel = mat4.create();
  mat4.rotateX(treeModel, treeModel, Math.PI / 2);
  mat4.rotateY(treeModel, treeModel, Math.random() * Math.PI * 2);
  treeScale = Math.random() + 1.5;
  mat4.scale(treeModel, treeModel, [treeScale, treeScale, treeScale]);
  let treeModel2 = mat4.create();
  mat4.rotateX(treeModel2, treeModel2, Math.PI / 2);
  mat4.rotateY(treeModel, treeModel, Math.random() * Math.PI * 2);
  treeScale = Math.random() + 1.5;
  mat4.scale(treeModel2, treeModel2, [treeScale, treeScale, treeScale]);
  mat4.translate(treeModel2, treeModel2, [0., 0., 40.]);
  let treeModel3 = mat4.create();
  mat4.rotateX(treeModel3, treeModel3, Math.PI / 2);
  mat4.rotateY(treeModel, treeModel, Math.random() * Math.PI * 2);
  treeScale = Math.random() + 1.5;
  mat4.scale(treeModel3, treeModel3, [treeScale, treeScale, treeScale]);
  mat4.translate(treeModel3, treeModel3, [8., 0., -17.]);
  let treeModel4 = mat4.create();
  mat4.rotateX(treeModel4, treeModel4, Math.PI / 2);
  mat4.rotateY(treeModel, treeModel, Math.random() * Math.PI * 2);
  treeScale = Math.random() + 1.5;
  mat4.scale(treeModel4, treeModel4, [treeScale, treeScale, treeScale]);
  mat4.translate(treeModel4, treeModel4, [0., 0., -24.]);
  let treeModel5 = mat4.create();
  mat4.rotateX(treeModel5, treeModel5, Math.PI / 2);
  mat4.rotateY(treeModel, treeModel, Math.random() * Math.PI * 2);
  treeScale = Math.random() + 1.5;
  mat4.scale(treeModel5, treeModel5, [treeScale, treeScale, treeScale]);
  mat4.translate(treeModel5, treeModel5, [16., 0., 15.]);
  let treeModel6 = mat4.create();
  mat4.rotateX(treeModel6, treeModel6, Math.PI / 2);
  mat4.rotateY(treeModel, treeModel, Math.random() * Math.PI * 2);
  treeScale = Math.random() + 1.5;
  mat4.scale(treeModel6, treeModel6, [treeScale, treeScale, treeScale]);
  mat4.translate(treeModel6, treeModel6, [-13., 0., 7.]);
  let treeModel7 = mat4.create();
  mat4.rotateX(treeModel7, treeModel7, Math.PI / 2);
  mat4.rotateY(treeModel, treeModel, Math.random() * Math.PI * 2);
  treeScale = Math.random() + 1.5;
  mat4.scale(treeModel7, treeModel7, [treeScale, treeScale, treeScale]);
  mat4.translate(treeModel7, treeModel7, [13., 0., -12.]);
  let treeModel8 = mat4.create();
  mat4.rotateX(treeModel8, treeModel8, Math.PI / 2);
  mat4.rotateY(treeModel, treeModel, Math.random() * Math.PI * 2);
  treeScale = Math.random() + 1.5;
  mat4.scale(treeModel8, treeModel8, [treeScale, treeScale, treeScale]);
  mat4.translate(treeModel8, treeModel8, [-8., 0., 12.]);
  let treeModel9 = mat4.create();
  mat4.rotateX(treeModel9, treeModel9, Math.PI / 2);
  mat4.rotateY(treeModel, treeModel, Math.random() * Math.PI * 2);
  treeScale = Math.random() + 1.5;
  mat4.scale(treeModel9, treeModel9, [treeScale, treeScale, treeScale]);
  mat4.translate(treeModel9, treeModel9, [-5., 0., 27.]);
  let treeModel10 = mat4.create();
  mat4.rotateX(treeModel10, treeModel10, Math.PI / 2);
  mat4.rotateY(treeModel, treeModel, Math.random() * Math.PI * 2);
  treeScale = Math.random() + 1.5;
  mat4.scale(treeModel10, treeModel10, [treeScale, treeScale, treeScale]);
  mat4.translate(treeModel10, treeModel10, [-8., 0., 15.]);

  // Load in the textures
  let grassTexture = lgl.load_texture_clamp(gl, grassTextureFile, gl.REPEAT) as WebGLTexture;
  let barkTexture = lgl.load_texture(gl, barkTextureFile) as WebGLTexture;
  let leafTexture = lgl.load_texture(gl, leafTextureFile) as WebGLTexture;

  let cietorgb = mat3.create();
  mat3.set(cietorgb, 3.2406, -1.5372, -0.4986, -0.9689, 1.8758, 0.0415, 0.0557, -0.2040, 1.0570)

  // Setup the cubemap
  var ct = 0;
  var img = new Array(6);

  let skyboxCubemap = gl.createTexture();

  for (var i = 0; i < 6; i++) {
    img[i] = new Image();
    img[i].onload = function() {
      ct++;
      if (ct == 6) {
        gl.bindTexture(gl.TEXTURE_CUBE_MAP, skyboxCubemap);
        var targets = [
          gl.TEXTURE_CUBE_MAP_POSITIVE_X, gl.TEXTURE_CUBE_MAP_NEGATIVE_X, 
          gl.TEXTURE_CUBE_MAP_POSITIVE_Y, gl.TEXTURE_CUBE_MAP_NEGATIVE_Y, 
          gl.TEXTURE_CUBE_MAP_POSITIVE_Z, gl.TEXTURE_CUBE_MAP_NEGATIVE_Z 
        ];
        for (var j = 0; j < 6; j++) {
          gl.texImage2D(targets[j], 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, img[j]);
          gl.texParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
          gl.texParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        }
        gl.generateMipmap(gl.TEXTURE_CUBE_MAP);
      }
    }
    img[i].src = urls2[i];
    // img[i].src = nightTexture;
  }

  // Build the shadowbuffer
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

  let time = .5;
  let sun = vec3.create();
  let sunOrigin = vec3.create();
  vec3.set(sun, 0., 30., 50.);
  vec3.set(sunOrigin, 0., 0., 0.);
  let lightProjectionMatrix = mat4.create();
  let range = 200;
  mat4.ortho(lightProjectionMatrix, -range, range, -range, range, 0.1, 200.);
  let lightViewMatrix = mat4.create();
  mat4.lookAt(lightViewMatrix, sun, [0., 0., 0.], [0., 1., 0.]);


  // Setup time management
  document.onkeypress = function (evt) {
    evt = evt || window.event;
    let charCode = evt.keyCode || evt.which;
    let charStr = String.fromCharCode(charCode);
    if (charStr == "a") {
      vec3.rotateY(sun, sun, sunOrigin, 1 / 60.);
      mat4.lookAt(lightViewMatrix, sun, [0., 0., 0.], [0., 1., 0.]);
      time += 1 / (2 * Math.PI * 60);
      if (time > 1.) {
        time -= 1.
      }
    }
    if (charStr == "d") {
      vec3.rotateY(sun, sun, sunOrigin, -1 / 60.);
      mat4.lookAt(lightViewMatrix, sun, [0., 0., 0.], [0., 1., 0.]);
      time -= 1 / (2 * Math.PI * 60);
      if (time < 0.) {
        time += 1.
      }
    }
    mat4.ortho(lightProjectionMatrix, -range, range, -range, range, 0.1, 200 + 100 * Math.abs(.5-time));
    console.log(time);
  }

  function renderSkyboxAndCubes(projection: mat4, view: mat4) {
    let view_no_transform = mat4.create();
    let temp = quat.create();
    mat4.getRotation(temp, view);
    mat4.fromQuat(view_no_transform, temp);

    // Draw the skybox, with its static cubemap texture.
    gl.useProgram(programSB);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjectionSB, false, projection);
    gl.uniformMatrix4fv(loc_uViewSB, false, view_no_transform);
    gl.uniformMatrix4fv(loc_uModelSB, false, skyboxModel);
    gl.uniform1f(loc_uTimeSB, time);
    gl.uniform3fv(loc_uLightSB, sun);
    // gl.uniformMatrix3fv(loc_uCIEtoRGB, false, cietorgb);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aPositionSB, skybox.positions, 3);
    gl.bindTexture(gl.TEXTURE_CUBE_MAP, skyboxCubemap);   

    // Draw the object.
    gl.disable(gl.CULL_FACE);
    lgl.drawMesh(gl, skybox);
    gl.enable(gl.CULL_FACE);
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
    function buildTreeShadow(treeModelGiven: mat4) { 
      buildShadowBuffers(tree, treeModelGiven);
      buildShadowBuffers(treeleaves, treeModelGiven);
    }
    buildTreeShadow(treeModel);
    buildTreeShadow(treeModel2);
    buildTreeShadow(treeModel3);
    buildTreeShadow(treeModel4);
    buildTreeShadow(treeModel5);
    buildTreeShadow(treeModel6);
    buildTreeShadow(treeModel7);
    buildTreeShadow(treeModel8);
    buildTreeShadow(treeModel9);
    buildTreeShadow(treeModel10);
    buildShadowBuffers(ground, groundModel)

    // Reset all the things
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.clearColor(0., 0., 0., 0.);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    let canvases = document.getElementsByTagName('canvas');
    if (canvases.length === 0) {
      throw "no canvas found";
    }
    let canvas = canvases[0] as HTMLCanvasElement;
    gl.viewport(0,0,canvas.width,canvas.height);

    renderSkyboxAndCubes(projection, view);
    gl.clear(gl.DEPTH_BUFFER_BIT);

    // Draw the objects
    gl.useProgram(uberprogram);

    gl.activeTexture(gl.TEXTURE1);
    gl.bindTexture(gl.TEXTURE_2D, shadowDepthTexture);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uLightView, false, lightViewMatrix);
    gl.uniformMatrix4fv(loc_uLightProjection, false, lightProjectionMatrix);
    gl.uniform3fv(loc_uLight, sun);
    gl.uniform1f(loc_uTime, time);
    gl.uniform1i(loc_uTexture, 0);
    gl.uniform1i(loc_uShadowTexture, 1);
   
    // Draw the objects
    gl.uniform1f(loc_uSpecStrength, 0.);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, grassTexture);
    lgl.bind_attrib_buffer(gl, loc_aPosition, ground.positions, 3);
    lgl.bind_attrib_buffer(gl, loc_aNormal, ground.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aTexCoord, ground.texcoords, 2);
    gl.uniformMatrix4fv(loc_uModel, false, groundModel);
    lgl.drawMesh(gl, ground);

    function drawTree(treeModelGiven : mat4) {
      gl.uniform1f(loc_uSpecStrength, 0.);
      gl.activeTexture(gl.TEXTURE0);
      gl.bindTexture(gl.TEXTURE_2D, barkTexture);
      lgl.bind_attrib_buffer(gl, loc_aPosition, tree.positions, 3);  
      lgl.bind_attrib_buffer(gl, loc_aNormal, tree.normals, 3);  
      lgl.bind_attrib_buffer(gl, loc_aTexCoord, tree.texcoords, 2);
      gl.uniformMatrix4fv(loc_uModel, false, treeModelGiven);
      lgl.drawMesh(gl, tree);
  
      gl.activeTexture(gl.TEXTURE0);
      gl.bindTexture(gl.TEXTURE_2D, leafTexture);
      lgl.bind_attrib_buffer(gl, loc_aPosition, treeleaves.positions, 3);
      lgl.bind_attrib_buffer(gl, loc_aNormal, treeleaves.normals, 3);  
      gl.uniform1f(loc_uSpecStrength, .2);
      lgl.bind_attrib_buffer(gl, loc_aTexCoord, treeleaves.texcoords, 2);
      gl.uniformMatrix4fv(loc_uModel, false, treeModelGiven);
      lgl.drawMesh(gl, treeleaves);
    }

    drawTree(treeModel);
    drawTree(treeModel2);
    drawTree(treeModel3);
    drawTree(treeModel4);
    drawTree(treeModel5);
    drawTree(treeModel6);
    drawTree(treeModel7);
    drawTree(treeModel8);
    drawTree(treeModel9);
    drawTree(treeModel10);
  }
}

main();