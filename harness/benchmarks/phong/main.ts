import * as lgl from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';

function main() {
  let [gl, params] = lgl.setup(render);
  const NUM_OBJECTS = parseInt(params['num_objects'] || "100");
  const SHADER = params['shader'] || 'default';
  const shaders = {
    'auto': [require('./auto/vertex.lgl'), require('./auto/fragment.lgl')],
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
    location['aPosition'] = lgl.attribLoc(gl, program, 'aPosition');
    location['aNormal'] = lgl.attribLoc(gl, program, 'aNormal');
    locations.push(location);
  });

  // We'll draw a teapot.
  let meshes = new Array(NUM_OBJECTS).fill(lgl.getBunny(gl));

  // Initialize the model position.
  let models: mat4[] = [];
  for (let i = 0; i < NUM_OBJECTS; i++)
    models.push(mat4.create());
  const block = Math.floor(Math.sqrt(NUM_OBJECTS));
  const OFFSET = 1;
  for (let i = 0; i < block; i++) {
    for (let j = 0; j < block; j++) {
      mat4.translate(models[i * block + j], models[i * block + j], [OFFSET * i, OFFSET * j, 0]);
    }
  }

  // Position the light source for the lighting effect.
  let light = vec3.fromValues(20., 0., 20.);
  function render(view: mat4, projection: mat4) {
    // Rotate the model a little bit on each frame.

    for (let i = 0; i < NUM_OBJECTS; i++) {
      mat4.rotateY(models[i], models[i], .01);

      // Use our shader pair.
      gl.useProgram(programs[i]);

      // Set the shader "uniform" parameters.
      gl.uniformMatrix4fv(locations[i]["uProjection"], false, projection);
      gl.uniformMatrix4fv(locations[i]["uView"], false, view);
      gl.uniformMatrix4fv(locations[i]["uModel"], false, models[i]);
      gl.uniform3fv(locations[i]["uLight"], light);

      // Set the attribute arrays.
      lgl.bind_attrib_buffer(gl, locations[i]["aNormal"] as number, meshes[i].normals, 3);
      lgl.bind_attrib_buffer(gl, locations[i]["aPosition"] as number, meshes[i].positions, 3);

      // Draw the object.
      lgl.drawMesh(gl, meshes[i]);
    }
  }
}

main();
