function addMousePort(elmApp) {

  document.addEventListener("mousedown", function(event) {
    elmApp.ports.mouseButton.send([event.button, true]);
  }, false);

  document.addEventListener("mouseup", function(event) {
    elmApp.ports.mouseButton.send([event.button, false]);
  }, false);
}
