module.exports = {
    entry: "./script.js",
    output: {
      filename: "bundle.js",
      library: 'WebpackTest',
      libraryTarget: 'var'
    },
    node: {
        module: "empty",
        net:"empty",
        fs: "empty"
    }
}