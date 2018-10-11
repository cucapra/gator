import * as lgl from '../lglexample';
import { mat4 } from 'gl-matrix';

function main() {
  let gl = lgl.setup(render);

  // Compile our shadowmap
  let shadowmap = lgl.compileProgram(gl,
    require('./shadowvertex.lgl'),
    require('./shadowfragment.lgl')
  );

  // Compile our shaders.
  let program = lgl.compileProgram(gl,
    require('./vertex.lgl'),
    require('./fragment.lgl')
  );

  // Uniform and attribute locations.
  let programLocations : { [locname : string]: WebGLUniformLocation | number; } = {};
  programLocations["uProjection"] = lgl.uniformLoc(gl, program, "uProjection");
  programLocations["uView"] = lgl.uniformLoc(gl, program, "uView");
  programLocations["uModel"] = lgl.uniformLoc(gl, program, "uModel");
  programLocations["uTexture"] = lgl.uniformLoc(gl, program, "uTexture");

  let shadowLocations : { [locname : string]: WebGLUniformLocation | number; } = {};
  shadowLocations["uProjection"] = lgl.uniformLoc(gl, shadowmap, "uProjection");
  shadowLocations["uView"] = lgl.uniformLoc(gl, shadowmap, "uView");
  shadowLocations["uModel"] = lgl.uniformLoc(gl, shadowmap, "uModel");
  //shadowLocations["uLight"] = lgl.uniformLoc(gl, shadowmap, "uLight");
  shadowLocations["aPosition"] = lgl.attribLoc(gl, shadowmap, "aPosition");
  //shadowLocations["aNormal"] = lgl.attribLoc(gl, shadowmap, "aNormal");

  // We'll draw a teapot over a plane with a wall next door
  let teapot = lgl.getTeapot(gl);
  //let plane = lgl.getCube(gl, 100, 100, .01, 1, 1);
  let plane = lgl.getCube(gl, 100, 100, .01, 1, 1);
  let wall = lgl.getCube(gl, 100, 50, .01, 1, 1);

  // Initialize the model positions.
  let teapotModel = mat4.create();
  //mat4.translate(teapotModel, teapotModel, [-30.+25., 0., 0.]);
  let planeModel = mat4.create();
  //mat4.translate(planeModel, planeModel, [25., -50., 0.]);
  //mat4.rotateX(planeModel, planeModel, Math.PI/2);
  let wallModel = mat4.create();
  mat4.translate(wallModel, wallModel, [-100./2.+25., -(100.-50.)/2, 0.]);
  mat4.rotateY(wallModel, wallModel, Math.PI/2);

  // Set the light position
  let light = [-30., 30., 0.];

  // Setup the shadow buffer
  let shadowFramebuffer = gl.createFramebuffer();
  gl.bindFramebuffer(gl.FRAMEBUFFER, shadowFramebuffer)
  let shadowDepthTextureWidth = 1024;
  let shadowDepthTextureHeight = 1024;

  let shadowDepthTexture = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, shadowDepthTexture);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
  //gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  //gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, shadowDepthTextureWidth, shadowDepthTextureHeight, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
  gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, shadowDepthTexture, 0);

  let projectionMatrix = mat4.create();
  mat4.perspective(projectionMatrix, Math.PI/2, 1.4, 1, 2000);
  let viewMatrix = mat4.create();
  mat4.lookAt(viewMatrix, [0., 0., -50.], [0., 0., 0.], [0., 1., 0.]);
  mat4.invert(viewMatrix, viewMatrix);

  /*var renderBuffer = gl.createRenderbuffer();
  gl.bindRenderbuffer(gl.RENDERBUFFER, renderBuffer);
  gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, shadowDepthTextureSize, shadowDepthTextureSize);

  gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, shadowDepthTexture, 0);
  gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, renderBuffer);
  gl.bindTexture(gl.TEXTURE_2D, null);
  gl.bindRenderbuffer(gl.RENDERBUFFER, null);
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);*/

  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.
    //mat4.rotateY(teapotModel, teapotModel, .01);

    function buildShadowBuffers(mesh: lgl.Mesh, model: mat4) {
      gl.bindFramebuffer(gl.FRAMEBUFFER, shadowFramebuffer);

      gl.viewport(0, 0, shadowDepthTextureWidth, shadowDepthTextureHeight);
      gl.clearColor(0, 0, 1, 1);
      gl.clearDepth(1.0);
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

      //lgl.bind_attrib_buffer(gl, shadowLocations["aNormal"] as number, mesh.normals, 3);
      gl.uniformMatrix4fv(shadowLocations["uModel"], false, model);
      lgl.bind_attrib_buffer(gl, shadowLocations["aPosition"] as number, mesh.positions, 3);

      lgl.drawMesh(gl, mesh);

      gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    }

    function drawObject(mesh: lgl.Mesh, model: mat4) {
      gl.uniformMatrix4fv(programLocations["uModel"], false, model);
      lgl.bind_attrib_buffer(gl, programLocations["aPosition"] as number, mesh.positions, 3);

      lgl.drawMesh(gl, mesh);
    }

    // Use our shader pair.
    gl.useProgram(shadowmap);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(shadowLocations["uProjection"], false, projectionMatrix);
    gl.uniformMatrix4fv(shadowLocations["uView"], false, viewMatrix);
    //gl.uniform3fv(shadowLocations["uLight"], light);

    buildShadowBuffers(teapot, teapotModel);
    let width = gl.drawingBufferWidth;
    let height = gl.drawingBufferHeight;
    gl.viewport(0, 0, width, height);
    gl.clearColor(0., 0., 0., 0.);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);


    gl.useProgram(program);

    gl.uniformMatrix4fv(programLocations["uProjection"], false, projection);
    gl.uniformMatrix4fv(programLocations["uView"], false, view);
    gl.bindTexture(gl.TEXTURE_2D, shadowDepthTexture);
    gl.uniform1i(programLocations["uTexture"], 0);
    
    drawObject(teapot, teapotModel);
    //drawObject(plane, planeModel);
    //drawObject(wall, wallModel);
  }
  
}

main();
