module.exports = {
  entry: './trivial.ts',
  mode: 'development',
  output: {
    filename: 'trivial.bundle.js'
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
    extensions: [ '.tsx', '.ts', '.js' ]
  },
};
