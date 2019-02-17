import * as lgl from '../lglexample';
import { mat4 } from 'gl-matrix';
import { debug } from 'util';

function main() {
  let [gl, params] = lgl.setup(render);
  let shadowDepthTextureSize = 1024
  const SHADER = params['shader'] || 'default';

  const shaders = {
    'default': {
      'shadow': [require('./default/shadowvertex.lgl'), require('./default/shadowfragment.lgl')],
      'program': [require('./default/vertex.lgl'), require('./default/fragment.lgl')],
    },
    'raw': {
      'shadow': [require('./raw/shadowvertex.glsl'), require('./raw/shadowfragment.glsl')],
      'program': [require('./raw/vertex.glsl'), require('./raw/fragment.glsl')],
    },
  };
  console.log(shaders['default'])
  // Compile our shadowmap
  let shadowmap = lgl.compileProgram(gl,
    shaders[SHADER]['shadow'][0],
    shaders[SHADER]['shadow'][1]
  );

  // Compile our shaders.
  let program = lgl.compileProgram(gl,
    shaders[SHADER]['program'][0],
    shaders[SHADER]['program'][1]
  );

  // Uniform and attribute locations.
  let programLocations: {
    [locname: string]: WebGLUniformLocation | number;
  } = {};
  programLocations["uProjection"] = lgl.uniformLoc(gl, program, "uProjection");
  programLocations["uCamera"] = lgl.uniformLoc(gl, program, "uCamera");
  programLocations["uLightProjection"] = lgl.uniformLoc(gl, program, "uLightProjection");
  programLocations["uLightView"] = lgl.uniformLoc(gl, program, "uLightView");
  programLocations["uModel"] = lgl.uniformLoc(gl, program, "uModel");
  programLocations["uTexture"] = lgl.uniformLoc(gl, program, "uTexture");
  programLocations["uLight"] = lgl.uniformLoc(gl, program, "uLight");
  programLocations["aPosition"] = lgl.attribLoc(gl, program, "aPosition");
  programLocations["aNormal"] = lgl.attribLoc(gl, program, "aNormal");
  programLocations["uBaseColor"] = lgl.uniformLoc(gl, program, "uBaseColor");
  programLocations["uSpecStrength"] = lgl.uniformLoc(gl, program, "uSpecStrength");

  let shadowLocations: { [locname: string]: WebGLUniformLocation | number; } = {};
  shadowLocations["uLightProjection"] = lgl.uniformLoc(gl, shadowmap, "uLightProjection");
  shadowLocations["uLightView"] = lgl.uniformLoc(gl, shadowmap, "uLightView");
  shadowLocations["uModel"] = lgl.uniformLoc(gl, shadowmap, "uModel");
  shadowLocations["aPosition"] = lgl.attribLoc(gl, shadowmap, "aPosition");

  // We'll draw a teapot over a plane with a wall next door
  let teapot = lgl.getTeapot(gl);
  let bunny = lgl.getBunny(gl);
  let plane = lgl.getCube(gl, 100, 100, 1, 1, 1);
  let wall = lgl.getCube(gl, 100, 50, 2, 1, 1);

  // Initialize the model positions.
  let teapotModel = mat4.create();
  mat4.translate(teapotModel, teapotModel, [0. + 25., 0., 20.]);
  let bunnyModel = mat4.create();
  mat4.translate(bunnyModel, bunnyModel, [-25. + 25., 0., -10.]);
  mat4.scale(bunnyModel, bunnyModel, [2., 2., 2.]);
  let planeModel = mat4.create();
  mat4.translate(planeModel, planeModel, [25., -25., 0.]);
  mat4.rotateX(planeModel, planeModel, Math.PI / 2);
  let wallModel = mat4.create();
  mat4.translate(wallModel, wallModel, [-100. / 2. + 25., -(50. - 50.) / 2, 0.]);
  mat4.rotateY(wallModel, wallModel, Math.PI / 2);

  // Set the light position
  let light = [-20., 30., 2.];

  // Setup the shadow buffer

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

  gl.bindTexture(gl.TEXTURE_2D, null)
  gl.bindRenderbuffer(gl.RENDERBUFFER, null)

  let lightProjectionMatrix = mat4.create();
  mat4.ortho(lightProjectionMatrix, -80, 80, -80, 80, -80.0, 160);
  let lightViewMatrix = mat4.create();
  mat4.lookAt(lightViewMatrix, light, [0., 0., 0.], [0., 1., 0.]);

  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.
    mat4.rotateY(teapotModel, teapotModel, .01);
    mat4.rotateZ(bunnyModel, bunnyModel, .01);

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

    function drawObject(mesh: lgl.Mesh, model: mat4) {
      gl.uniformMatrix4fv(programLocations["uModel"], false, model);
      lgl.bind_attrib_buffer(gl, programLocations["aPosition"] as number, mesh.positions, 3);
      lgl.bind_attrib_buffer(gl, programLocations["aNormal"] as number, mesh.normals, 3);

      lgl.drawMesh(gl, mesh);
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
    buildShadowBuffers(teapot, teapotModel);
    buildShadowBuffers(bunny, bunnyModel);
    buildShadowBuffers(wall, wallModel);
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);

    let width = gl.drawingBufferWidth;
    let height = gl.drawingBufferHeight;
    gl.viewport(0, 0, width, height);
    gl.clearColor(0., 0., 0., 0.);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    gl.useProgram(program);

    gl.uniformMatrix4fv(programLocations["uProjection"], false, projection);
    gl.uniformMatrix4fv(programLocations["uCamera"], false, view);
    gl.uniformMatrix4fv(programLocations["uLightProjection"], false, lightProjectionMatrix);
    gl.uniformMatrix4fv(programLocations["uLightView"], false, lightViewMatrix);
    gl.uniform3fv(programLocations["uLight"], light);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, shadowDepthTexture);
    gl.uniform1i(programLocations["uTexture"], 0);

    gl.uniform3fv(programLocations["uBaseColor"], [2., 2.5, 5.]);
    gl.uniform1f(programLocations["uSpecStrength"], 5.);
    drawObject(teapot, teapotModel);
    gl.uniform3fv(programLocations["uBaseColor"], [5., 2.5, 2.]);
    gl.uniform1f(programLocations["uSpecStrength"], .01);
    drawObject(bunny, bunnyModel);
    gl.uniform3fv(programLocations["uBaseColor"], [3., 3., 2.]);
    gl.uniform1f(programLocations["uSpecStrength"], .3);
    drawObject(plane, planeModel);
    drawObject(wall, wallModel);
  }

}

main();
