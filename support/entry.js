var ClientEntry = require("../src/Main.purs");
var app = ClientEntry.main();

// If hot-reloading, hook into each state change and re-render using the last
// state.
if (module.hot) {
  module.hot.accept();
}
