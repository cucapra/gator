module.exports = function (bundler) {
  bundler.addAssetType('lgl', require.resolve('./LinguineAsset'));
};
