import * as lgl from '../lglexample';
import { mat4, mat3, vec3, vec4 } from 'gl-matrix';

function main() {
  class MovingCube {
    translation: vec3;
    localRotationAxis: vec3;
    localAngularVelocity: number;
    globalRotationAxis: vec3;
    globalAngularVelocity: number;
    color: vec3;

    constructor(i: number){
      this.translation = vec3.fromValues(Math.random()-.5,Math.random()-.5,Math.random()-.5);
      this.localRotationAxis = vec3.fromValues(Math.random(),Math.random(),Math.random());
      this.localAngularVelocity = 0.005 + 0.1*Math.random();
      this.globalRotationAxis = vec3.fromValues(Math.random(),Math.random(),Math.random());
      this.globalAngularVelocity = 0.005 + 0.02*Math.random();
      this.color = vec3.fromValues(Math.random(),Math.random(),Math.random());
    }
  }

  let frameNumber = 0;

  let gl = lgl.setup(render);

  // Compile our shaders.
  let programSB = lgl.compileProgram(gl,
    require('./vertexSB.lgl'),
    require('./fragmentSB.lgl')
  );

  // Uniform and attribute locations.
  let loc_uProjectionSB = lgl.uniformLoc(gl, programSB, 'uProjection');
  let loc_uViewSB = lgl.uniformLoc(gl, programSB, 'uView');
  let loc_uModelSB = lgl.uniformLoc(gl, programSB, 'uModel');
  let loc_aPositionSB = lgl.attribLoc(gl, programSB, 'aPosition');

  // We'll draw a cube.
  let skybox = lgl.getCube(gl, 300, 300, 300, 1, 1);

  // Initialize the model position.
  let modelSB = mat4.create();

  // Compile our shaders.
  let programOBJ = lgl.compileProgram(gl,
    require('./vertexOBJ.lgl'),
    require('./fragmentOBJ.lgl')
  );

  gl.useProgram(programOBJ);

  // Uniform and attribute locations.
  let loc_uProjectionOBJ = lgl.uniformLoc(gl, programOBJ, 'uProjection');
  let loc_uViewOBJ = lgl.uniformLoc(gl, programOBJ, 'uView');
  let loc_uModelOBJ = lgl.uniformLoc(gl, programOBJ, 'uModel');
  let loc_uLightOBJ = lgl.uniformLoc(gl, programOBJ, 'uLight');
  // let loc_uColorOBJ = lgl.uniformLoc(gl, programOBJ, 'uColor');
  let loc_aPositionOBJ = lgl.attribLoc(gl, programOBJ, 'aPosition');
  let loc_aNormalOBJ = lgl.attribLoc(gl, programOBJ, 'aNormal');

  let cubeMesh = lgl.getCube(gl, 10, 10, 10, 1, 1);

  let movingCubeData: MovingCube[] = [];
  for (var i = 0; i <= 3; i++) {
    movingCubeData.push(new MovingCube(i));
    vec3.normalize(movingCubeData[i].localRotationAxis, movingCubeData[i].localRotationAxis);
    vec3.normalize(movingCubeData[i].globalRotationAxis, movingCubeData[i].globalRotationAxis);
    vec3.normalize(movingCubeData[i].translation, movingCubeData[i].translation);
    vec3.scale(movingCubeData[i].translation, movingCubeData[i].translation, 30);
    if (Math.random() < 0.5) {
      movingCubeData[i].globalAngularVelocity *= -1;
    }
  }

  // Compile our shaders.
  let program = lgl.compileProgram(gl,
    require('./vertex.lgl'),
    require('./fragment.lgl')
  );

  // Uniform and attribute locations.
  let loc_uProjection = lgl.uniformLoc(gl, program, 'uProjection');
  let loc_uView = lgl.uniformLoc(gl, program, 'uView');
  let loc_uModel = lgl.uniformLoc(gl, program, 'uModel');
  let loc_uNormalMatrix = lgl.uniformLoc(gl, program, 'uNormalMatrix');
  let loc_uInverseViewTransform = lgl.uniformLoc(gl, program, 'uInverseViewTransform');
  let loc_aPosition = lgl.attribLoc(gl, program, 'aPosition');
  let loc_aNormal = lgl.attribLoc(gl, program, 'aNormal');

  // We'll draw a teapot.
  let teapot = lgl.getBunny(gl);

  // Initialize the model position.
  let model = mat4.create();

  mat4.scale(model, model, [4.0, 4., 4.]);

  var ct = 0;
  var img = new Array(6);
  var urls = [
      require('../resources/park/posx.jpg'), require('../resources/park/negx.jpg'),
      require('../resources/park/posy.jpg'), require('../resources/park/negy.jpg'),
      require('../resources/park/posz.jpg'), require('../resources/park/negz.jpg')
  ];

  let cubemapTargets = [  // targets for use in some gl functions for working with cubemaps
    gl.TEXTURE_CUBE_MAP_POSITIVE_X, gl.TEXTURE_CUBE_MAP_NEGATIVE_X,
    gl.TEXTURE_CUBE_MAP_POSITIVE_Y, gl.TEXTURE_CUBE_MAP_NEGATIVE_Y,
    gl.TEXTURE_CUBE_MAP_POSITIVE_Z, gl.TEXTURE_CUBE_MAP_NEGATIVE_Z
  ];

  let dynamicCubemap = gl.createTexture(); // Create the texture object for the reflection map

  gl.bindTexture(gl.TEXTURE_CUBE_MAP, dynamicCubemap);  // create storage for the reflection map images
  for (i = 0; i < 6; i++) {
    gl.texImage2D(cubemapTargets[i], 0, gl.RGBA, 512, 512, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
    //With null as the last parameter, the previous function allocates memory for the texture and fills it with zeros.
  }

  let skyboxCubemap = gl.createTexture();  // Create the texture object for the skybox

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
    img[i].src = urls[i];
  }

  let frameBuffer = gl.createFramebuffer();  // create the framebuffer that will draw to the reflection map
  gl.bindFramebuffer(gl.FRAMEBUFFER,frameBuffer);  // select the framebuffer, so we can attach the depth buffer to it
  let depthBuffer = gl.createRenderbuffer();   // renderbuffer for depth buffer in framebuffer
  gl.bindRenderbuffer(gl.RENDERBUFFER, depthBuffer); // so we can create storage for the depthBuffer
  gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, 512, 512);
  gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, depthBuffer);

  function renderSkyboxAndCubes(projection: mat4, view: mat4) {

    gl.clearColor(0,0,0,1);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    // Draw the skybox, with its static cubemap texture.
    gl.useProgram(programSB);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjectionSB, false, projection);
    gl.uniformMatrix4fv(loc_uViewSB, false, view);
    gl.uniformMatrix4fv(loc_uModelSB, false, modelSB);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aPositionSB, skybox.positions, 3);

    gl.clearColor(0,0,0,1);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    gl.bindTexture(gl.TEXTURE_CUBE_MAP, skyboxCubemap);

    // Draw the object.
    gl.disable(gl.CULL_FACE);
    lgl.drawMesh(gl, skybox);
    gl.enable(gl.CULL_FACE);

    // Draw the moving cubes, which are drawn with lighting.
    gl.useProgram(programOBJ);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aNormal, cubeMesh.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aPosition, cubeMesh.positions, 3);

    let modelview = mat4.create();
    mat4.multiply(modelview, model, view);

    // Position the light source for the lighting effect.
    let light = vec4.fromValues(0., 30., 0., 0.);

    let transformed = vec4.create();
    vec4.transformMat4(transformed, light, modelview);

    gl.uniform4fv( loc_uLightOBJ, transformed );

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjectionOBJ, false, projection);
    gl.uniformMatrix4fv(loc_uViewOBJ, false, view);

    for (var i = 0; i < movingCubeData.length; i++) {  // draw the cubes
        var cd = movingCubeData[i];
        let cdModel = mat4.create();
        mat4.rotate(cdModel, model, frameNumber*cd.globalAngularVelocity, cd.globalRotationAxis);
        mat4.translate(cdModel, cdModel, cd.translation);
        mat4.rotate(cdModel, cdModel, frameNumber*cd.localAngularVelocity, cd.localRotationAxis);

        gl.uniformMatrix4fv(loc_uModelOBJ, false, cdModel);
        // gl.uniform3fv(loc_uColorOBJ, cd.color);

        // Draw the cube.
        lgl.drawMesh(gl, cubeMesh);
    }

    gl.disableVertexAttribArray(loc_aPositionOBJ);
    gl.disableVertexAttribArray(loc_aNormalOBJ);
  }

  function createDynamicCubemap() {
    gl.bindFramebuffer(gl.FRAMEBUFFER, frameBuffer);
    gl.viewport(0,0,512,512);  //match size of the texture images
    let projection = mat4.create();
    mat4.perspective(projection, Math.PI/2, 1, 1, 300);  // Set projection to give 90-degree field of view.

    let view = mat4.create();

    mat4.identity(view);
    mat4.scale(view,view,[-1,-1,1]);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_CUBE_MAP_NEGATIVE_Z, dynamicCubemap, 0);
    renderSkyboxAndCubes(projection, view);

    mat4.identity(view);
    mat4.scale(view,view,[-1,-1,1]);
    mat4.rotateY(view,view,Math.PI/2);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_CUBE_MAP_POSITIVE_X, dynamicCubemap, 0);
    renderSkyboxAndCubes(projection, view);

    mat4.identity(view);
    mat4.scale(view,view,[-1,-1,1]);
    mat4.rotateY(view,view,Math.PI);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_CUBE_MAP_POSITIVE_Z, dynamicCubemap, 0);
    renderSkyboxAndCubes(projection, view);

    mat4.identity(view);
    mat4.scale(view,view,[-1,-1,1]);
    mat4.rotateY(view,view,-Math.PI/2);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_CUBE_MAP_NEGATIVE_X, dynamicCubemap, 0);
    renderSkyboxAndCubes(projection, view);

    mat4.identity(view);
    mat4.rotateX(view,view,Math.PI/2);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_CUBE_MAP_NEGATIVE_Y, dynamicCubemap, 0);
    renderSkyboxAndCubes(projection, view);

    mat4.identity(view);
    mat4.rotateX(view,view,-Math.PI/2);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_CUBE_MAP_POSITIVE_Y, dynamicCubemap, 0);
    renderSkyboxAndCubes(projection, view);

    /* The commented out section below is an alternative way of computing the positive and negative Y images,
       including the x/y flip.  The rotations that are used in this version correspond are the correct rotations
       based on the layout of the six images in a cubemap.   The single rotation used above is equivalent to the
       flip and two rotations used below. */

    //mat4.identity(modelview);
    //mat4.scale(modelview,modelview,[-1,-1,1]);
    //mat4.rotateX(modelview,modelview,Math.PI/2);
    //mat4.rotateY(modelview,modelview,Math.PI);
    //gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_CUBE_MAP_NEGATIVE_Y, dynamicCubemap, 0);
    //renderSkyboxAndCubes();
    //
    //mat4.identity(modelview);
    //mat4.scale(modelview,modelview,[-1,-1,1]);
    //mat4.rotateX(modelview,modelview,-Math.PI/2);
    //mat4.rotateY(modelview,modelview,Math.PI);
    //gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_CUBE_MAP_POSITIVE_Y, dynamicCubemap, 0);
    //renderSkyboxAndCubes();

    gl.bindTexture(gl.TEXTURE_CUBE_MAP, dynamicCubemap);
    gl.generateMipmap( gl.TEXTURE_CUBE_MAP );
  }

  function render(view: mat4, projection: mat4) {
    createDynamicCubemap();

    gl.bindFramebuffer(gl.FRAMEBUFFER, null);  // draw to screen
    let canvases = document.getElementsByTagName('canvas');
    if (canvases.length === 0) {
      throw "no canvas found";
    }
    let canvas = canvases[0] as HTMLCanvasElement;
    gl.viewport(0,0,canvas.width,canvas.height);

    renderSkyboxAndCubes(projection, view);

    // Use our shader pair.
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, model);

    let modelView = mat4.create();
    let normalMatrix = mat3.create();
    mat4.multiply(modelView, view, model);
    mat3.normalFromMat4(normalMatrix, modelView);
    gl.uniformMatrix3fv(loc_uNormalMatrix, false, normalMatrix);

    let inverseViewTransform = mat3.create();
    mat3.fromMat4(inverseViewTransform, modelView);
    mat3.invert(inverseViewTransform, inverseViewTransform);
    gl.uniformMatrix3fv(loc_uInverseViewTransform, false, inverseViewTransform);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aPosition, teapot.positions, 3);
    lgl.bind_attrib_buffer(gl, loc_aNormal, teapot.normals, 3);

    gl.bindTexture(gl.TEXTURE_CUBE_MAP, dynamicCubemap);

    // Draw the object.
    lgl.drawMesh(gl, teapot);

    frameNumber++;
  }
}

main();