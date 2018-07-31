const Asset = require('parcel-bundler/src/Asset');
const serializeObject = require('parcel-bundler/src/utils/serializeObject');

const util = require('util');
const child_process = require('child_process');

const execFile = util.promisify(child_process.execFile);

class LinguineAsset extends Asset {
  constructor(name, options) {
    super(name, options);
    this.type = 'js';
  }

  async load() {
    // Compile the code. (Eventually, this should probably go in `parse`
    // instead, once the compiler supports reading code from stdin...)
    let { stdout } = await execFile('lingc', [this.name]);
    return stdout;
  }

  async parse(code) {
    // "Parsing" is currently a no-op: we just return the compiled GLSL code
    // unchanged.
    return code;
  }

  async generate() {
    return serializeObject(
      this.ast,  // The result of `parse` is put into `this.ast`.
      this.options.minify && !this.options.scopeHoist
    );
  }
}

module.exports = LinguineAsset;
