import * as lgl from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';

var __dirname : string;

function main() {
  let [gl, params] = lgl.setup(render);
  const NUM_OBJECTS = parseInt(params['num_objects'] || "100");
  const SHADER = params['shader'] || 'default';

  const shaders = {
    'default': [require('./default/vertex.lgl'), require('./default/fragment.lgl')],
    'raw': [require('./raw/vertex.glsl'), require('./raw/fragment.glsl')]
  };
  const vertices: string[] = new Array(NUM_OBJECTS).fill(shaders[SHADER][0]);
  const frags: string[] = new Array(NUM_OBJECTS).fill(shaders[SHADER][1]);
  const programs: WebGLProgram[] = [];
  for (let i = 0; i < NUM_OBJECTS; i++) {
    programs.push(lgl.compileProgram(gl, vertices[i], frags[i]));
  }

  let locations: { [locname: string]: WebGLUniformLocation | number; }[] = [];
  programs.forEach((program) => {
    // Uniform and attribute locations.
    let location: { [locname: string]: WebGLUniformLocation | number; } = {}
    location['uProjection'] = lgl.uniformLoc(gl, program, 'uProjection');
    location['uView'] = lgl.uniformLoc(gl, program, 'uView');
    location['uModel'] = lgl.uniformLoc(gl, program, 'uModel');
    location['uLight'] = lgl.uniformLoc(gl, program, 'uLight');
    location['uLightTrans'] = lgl.uniformLoc(gl, program, 'uLightTrans');
    location['diffColor'] = lgl.uniformLoc(gl, program, 'diffColor');
    location['aPosition'] = lgl.attribLoc(gl, program, 'aPosition');
    location['aNormal'] = lgl.attribLoc(gl, program, 'aNormal');
    locations.push(location);
  
  });

  // We'll draw a gator
  // let mesh = lgl.getBunny(gl);
  let mesh = lgl.getTeapot(gl);
  let bunny = lgl.getBunny(gl);
  let plane = lgl.getCube(gl, 20, 20, 1, 1, 1);
  // Initialize the model position.
  let model = mat4.create();
  mat4.translate(model, model, [-5., 0., 0.]);
  mat4.scale(model, model, [.5, .5, .5]);

  let lightTrans = mat4.create();

  let bunnyModel = mat4.create();
  mat4.translate(bunnyModel, bunnyModel, [5., 0., 0.]);
  mat4.rotateY(bunnyModel, bunnyModel, Math.PI / 4);
  mat4.rotateX(bunnyModel, bunnyModel, Math.PI / 4);
  mat4.scale(bunnyModel, bunnyModel, [.5, .5, .5]);

  let planeModel = mat4.create();
  mat4.translate(planeModel, planeModel, [0., 0., -5.]);

  // Position the light source for the lighting effect.
  let light = vec3.fromValues(5., 0., 6.);

  function render(view: mat4, projection: mat4) {
    for (let i = 0; i < NUM_OBJECTS; i++) {
      // Rotate the model a little bit on each frame.
      mat4.rotateY(model, model, .01);
      mat4.rotateZ(bunnyModel, bunnyModel, .01);
      // mat4.rotateZ(lightTrans, lightTrans, .01);

      // Use our shader pair.
      gl.useProgram(programs[i]);

      // Set the shader "uniform" parameters.
      gl.uniformMatrix4fv(locations[i]["uProjection"], false, projection);
      gl.uniformMatrix4fv(locations[i]["uView"], false, view);
      gl.uniformMatrix4fv(locations[i]["uModel"], false, model);
      gl.uniformMatrix4fv(locations[i]["uLightTrans"], false, lightTrans);
      gl.uniform3fv(locations[i]["uLight"], light);
      gl.uniform3fv(locations[i]["diffColor"], [.4, .2, .6]);

      // Set the attribute arrays.
      lgl.bind_attrib_buffer(gl, locations[i]["aNormal"] as number, mesh.normals, 3);
      lgl.bind_attrib_buffer(gl, locations[i]["aPosition"] as number, mesh.positions, 3);

      gl.disable(gl.CULL_FACE);

      // Draw the object.
      lgl.drawMesh(gl, mesh);
      
      // Set the attribute arrays.
      gl.uniformMatrix4fv(locations[i]["uModel"], false, bunnyModel);
      lgl.bind_attrib_buffer(gl, locations[i]["aNormal"] as number, bunny.normals, 3);
      lgl.bind_attrib_buffer(gl, locations[i]["aPosition"] as number, bunny.positions, 3);
      lgl.drawMesh(gl, bunny);

      // Set the attribute arrays.
      gl.uniformMatrix4fv(locations[i]["uModel"], false, planeModel);
      lgl.bind_attrib_buffer(gl, locations[i]["aNormal"] as number, plane.normals, 3);
      lgl.bind_attrib_buffer(gl, locations[i]["aPosition"] as number, plane.positions, 3);
      lgl.drawMesh(gl, plane);
    }
  }
}

main();
