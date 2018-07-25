const Asset = require('parcel-bundler/src/Asset');

module.exports = class LinguineAsset extends Asset {
  constructor(name, options) {
    super(name, options);
    this.type = 'lgl';
  }

  async parse(code) {
    return 'parsed';
  }

  async generate() {
    return {
      'type': 'lgl',
      'value': 'text',
    };
  }
}
