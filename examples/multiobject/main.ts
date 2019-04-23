import * as lgl from '../lglexample';
import { mat4, vec3 } from 'gl-matrix';

let PROGRAM_COUNT = 4

function main() {
  let vertices : string[] = [];
  vertices.push(require('./vertex.lgl'));

  let fragments : string[] = [];
  fragments.push(require('./frag_main.lgl'));
  fragments.push(require('./frag_red_light.lgl'));
  fragments.push(require('./frag_green_light.lgl'));
  fragments.push(require('./frag_blue_light.lgl'));

  let gl = lgl.setup(render);

  // Compile our shaders.
  let programs : WebGLProgram[] = []
  for (let i = 0; i < PROGRAM_COUNT; i++) {
    programs.push(lgl.compileProgram(gl, vertices[0], fragments[i]))
    console.log(i);
  }

  // Uniform and attribute locations.
  let locations : { [locname : string]: WebGLUniformLocation | number; }[] = []
  programs.forEach(function(program) {
    let location : { [locname : string]: WebGLUniformLocation | number; } = {}
    location["uProjection"] = lgl.uniformLoc(gl, program, "uProjection")
    location["uView"] = lgl.uniformLoc(gl, program, "uView")
    location["uModel"] = lgl.uniformLoc(gl, program, "uModel")
    location["aPosition"] = lgl.attribLoc(gl, program, "aPosition")
    location["aNormal"] = lgl.attribLoc(gl, program, "aNormal")
    locations.push(location)
  })

  // We'll draw a teapot.
  let teapot = lgl.getTeapot(gl);

  // Initialize the model positions.
  let models : mat4[] = [];
  for (let i = 0; i < PROGRAM_COUNT; i++) {
    models.push(mat4.create());
  }

  // Setup the 'lights'
  let lights : vec3[] = [vec3.create(), vec3.create(), vec3.create(), vec3.create()];
  vec3.set(lights[1], 15., 0., 0.);
  vec3.set(lights[2], 0., -12., 12.);
  vec3.set(lights[3], 0., 12., -12.);

  // Create a bunch of constants for scaling
  let baseline = mat4.create();
  let lightScale = 10.;
  let scaleVec = vec3.create();
  vec3.set(scaleVec, lightScale, lightScale, lightScale);
  let scaledLight = vec3.create();
  mat4.scale(baseline, baseline, [1 / lightScale, 1 / lightScale, 1 / lightScale]);

  function render(view: mat4, projection: mat4) {
    for (let i = 0; i < PROGRAM_COUNT; i++) {
      if (i > 0) {
        vec3.multiply(scaledLight, lights[i], scaleVec);
        mat4.translate(models[i], baseline, scaledLight);
      }
      mat4.rotateY(baseline, baseline, .01 * i);

      // Use our shader pair.
      gl.useProgram(programs[i]);

      gl.uniformMatrix4fv(locations[i]["uProjection"], false, projection);
      gl.uniformMatrix4fv(locations[i]["uView"], false, view);
      gl.uniformMatrix4fv(locations[i]["uModel"], false, models[i]);

      // Set the attribute arrays.
      lgl.bind_attrib_buffer(gl, locations[i]["aPosition"] as number, teapot.positions, 3);
      lgl.bind_attrib_buffer(gl, locations[i]["aNormal"] as number, teapot.normals, 3);

      // Set the shader "uniform" parameters.
      lgl.drawMesh(gl, teapot);
    }
  }
}

main();
