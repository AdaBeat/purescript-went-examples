code = require("./output/Main/index.js")
// see https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function() {
    code.main();
  });
}
code.main();
