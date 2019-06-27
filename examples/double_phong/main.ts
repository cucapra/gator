import * as lgl from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';

function main() {
  let gl = lgl.setup(render);

  // Compile our shaders.
  let program1 = lgl.compileProgram(gl,
    require('./vertex1.lgl'),
    require('./fragment1.lgl')
  );

  let program2 = lgl.compileProgram(gl,
    require('./vertex2.lgl'),
    require('./fragment2.lgl')
  );

  // Uniform and attribute locations.
  let loc_uProjection1 = lgl.uniformLoc(gl, program1, 'uProjection');
  let loc_uView1 = lgl.uniformLoc(gl, program1, 'uView');
  let loc_uModel1 = lgl.uniformLoc(gl, program1, 'uModel');
  let loc_uLight1 = lgl.uniformLoc(gl, program1, 'uLight');
  let loc_aPosition1 = lgl.attribLoc(gl, program1, 'aPosition');
  let loc_aNormal1 = lgl.attribLoc(gl, program1, 'aNormal');

  let loc_uProjection2 = lgl.uniformLoc(gl, program2, 'uProjection');
  let loc_uView2 = lgl.uniformLoc(gl, program2, 'uView');
  let loc_uModel2 = lgl.uniformLoc(gl, program2, 'uModel');
  let loc_uLight2 = lgl.uniformLoc(gl, program2, 'uLight');
  let loc_aPosition2 = lgl.attribLoc(gl, program2, 'aPosition');
  let loc_aNormal2 = lgl.attribLoc(gl, program2, 'aNormal');

  // We'll draw a teapot.
  let mesh = lgl.getBunny(gl);

  // Initialize the model position.
  let model1 = mat4.create();
  mat4.translate(model1, model1, [-7., -7., 0.]);
  let unmoved = true;
  let model2 = mat4.create();
  mat4.translate(model2, model2, [-100, -100., 0.]);

  // Position the light source for the lighting effect.
  let light1 = vec3.fromValues(0., 0., 20.);
  let light2 = vec3.create();
  vec3.add(light2, light1, vec3.fromValues(7., 0., 0.))

  document.onkeypress = function (evt) {
    evt = evt || window.event;
    let charCode = evt.keyCode || evt.which;
    let charStr = String.fromCharCode(charCode);
    if (charStr == "a" && unmoved) {
      unmoved = false;
      model1 = mat4.create();
      mat4.translate(model1, model1, [-7., -7., 0.]);
      model2 = mat4.create();
      mat4.translate(model2, model2, [7, -7., 0.]);
    }
  }

  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.
    mat4.rotateY(model1, model1, .01);
    mat4.rotateY(model2, model2, .01);

    // Use our shader pair.
    gl.useProgram(program1);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection1, false, projection);
    gl.uniformMatrix4fv(loc_uView1, false, view);
    gl.uniformMatrix4fv(loc_uModel1, false, model1);
    gl.uniform3fv(loc_uLight1, light1);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aNormal1, mesh.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aPosition1, mesh.positions, 3);

    // Draw the object.
    lgl.drawMesh(gl, mesh);

    // Use our shader pair.
    gl.useProgram(program2);

    // Set the shader "uniform" parameters.
    gl.uniformMatrix4fv(loc_uProjection2, false, projection);
    gl.uniformMatrix4fv(loc_uView2, false, view);
    gl.uniformMatrix4fv(loc_uModel2, false, model2);
    gl.uniform3fv(loc_uLight2, light2);

    // Set the attribute arrays.
    lgl.bind_attrib_buffer(gl, loc_aNormal2, mesh.normals, 3);
    lgl.bind_attrib_buffer(gl, loc_aPosition2, mesh.positions, 3);

    // Draw the object.
    lgl.drawMesh(gl, mesh);
  }
}

main();
