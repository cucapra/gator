import * as lgl from '../lglexample';
import { mat4, mat3 } from 'gl-matrix';

function main() {
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
  let skybox = lgl.getCube(gl, 200, 200, 200, 1, 1);

  // Initialize the model position.
  let modelSB = mat4.create();

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

  // We'll draw a cube.
  let teapot = lgl.getTeapot(gl);

  // Initialize the model position.
  let model = mat4.create();

  let normalMatrix = mat3.create();
  let inverseViewTransform = mat3.create();

  var ct = 0;
  var img = new Array(6);
  var urls = [
      require('../resources/park/posx.jpg'), require('../resources/park/negx.jpg'), 
      require('../resources/park/posy.jpg'), require('../resources/park/negy.jpg'), 
      require('../resources/park/posz.jpg'), require('../resources/park/negz.jpg')
  ];
  for (var i = 0; i < 6; i++) {
      img[i] = new Image();
      img[i].onload = function() {
          console.log(ct);
          ct++;
          if (ct == 6) {
              let texID = gl.createTexture();
              gl.bindTexture(gl.TEXTURE_CUBE_MAP, texID);
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

  function render(view: mat4, projection: mat4) {
    // Use our shader pair.
    gl.useProgram(programSB);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjectionSB, false, projection);
    gl.uniformMatrix4fv(loc_uViewSB, false, view);
    gl.uniformMatrix4fv(loc_uModelSB, false, modelSB);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aPositionSB, skybox.positions, 3);

    gl.clearColor(0,0,0,1);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    // Draw the object.
    gl.disable(gl.CULL_FACE);
    lgl.drawMesh(gl, skybox);
    gl.enable(gl.CULL_FACE);

    // Use our shader pair.
    gl.useProgram(program);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection, false, projection);
    gl.uniformMatrix4fv(loc_uView, false, view);
    gl.uniformMatrix4fv(loc_uModel, false, model);

    let modelView = mat4.create();
    mat4.multiply(modelView, view, model);
    mat3.normalFromMat4(normalMatrix, modelView);
    gl.uniformMatrix3fv(loc_uNormalMatrix, false, normalMatrix);

    mat3.fromMat4(inverseViewTransform, modelView);
    mat3.invert(inverseViewTransform, inverseViewTransform);
    gl.uniformMatrix3fv(loc_uInverseViewTransform, false, inverseViewTransform);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aPosition, teapot.positions, 3);
    lgl.bind_attrib_buffer(gl, loc_aNormal, teapot.normals, 3);

    // Draw the object.
    lgl.drawMesh(gl, teapot);
  }
}

main();