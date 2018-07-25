const Asset = require('parcel-bundler/src/Asset');
const serializeObject = require('parcel-bundler/src/utils/serializeObject');

class LinguineAsset extends Asset {
  constructor(name, options) {
    super(name, options);
    this.type = 'js';
  }

  async load() {
    return 'loaded';
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
