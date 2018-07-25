const Asset = require('parcel-bundler/src/Asset');
const serializeObject = require('../utils/serializeObject');

class LinguineAsset extends Asset {
  constructor(name, options) {
    super(name, options);
    console.error("CONSTRUCT", name);
    this.type = 'js';
  }

  async parse(code) {
    console.error("PARSE", code);
    return 'parsed';
  }

  async generate() {
    console.error("GENERATE");
    return serializeObject(
      "SOME DATA",
      this.options.minify && !this.options.scopeHoist
    );
  }
}

module.exports = LinguineAsset;
