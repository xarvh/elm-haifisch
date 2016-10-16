function playSoundPort(elmApp) {
    elmApp.ports.playSound.subscribe(function (soundName) {
        if (soundName) new Audio("sounds/" + soundName + ".ogg").play();
    });
}
