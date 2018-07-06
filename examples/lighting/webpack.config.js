module.exports = {
  entry: './build/lighting/lighting.js',
  mode: 'development',
  output: {
    filename: 'lighting.bundle.js'
  },
  node: {
    fs: 'empty'
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      }
    ]
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js']
  },
};
