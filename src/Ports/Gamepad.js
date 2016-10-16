function animationFrameAndGamepadsPort(elmApp) {

    var supportsGamepads =
        typeof navigator !== 'undefined' &&
        typeof navigator.getGamepads === 'function';

    var previousTimestamp = performance.now();

    raf();

    function raf() {
            requestAnimationFrame(function (timestamp) {

                var gpInList =
                    supportsGamepads ? navigator.getGamepads() : [];

                var gpOutList =
                    [];

                for (var i = 0; i < gpInList.length; i++) {

                    var gpIn =
                        gpInList[i];

                    if (gpIn && gpIn.connected) {
                        gpOutList.push({
                            index: gpIn.index,
                            axes: gpIn.axes || [],
                            buttons: (gpIn.buttons || []).map(function (b) { return [ b.pressed, b.value ]; }),
                        });
                    }
                }

                elmApp.ports.animationFrameAndGamepads.send({dt: timestamp - previousTimestamp, gamepads: gpOutList});

                previousTimestamp = timestamp;

                raf();
            });
    }
}
