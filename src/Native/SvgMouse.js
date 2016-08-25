var _xarvh$elm_haifisch$Native_SvgMouse = function() {

    function transform(selector, position) {

        try {
            var svg = document.querySelector(selector);
            var pt = svg.createSVGPoint();

            pt.x = position.x;
            pt.y = position.y;
            var value = pt.matrixTransform(svg.getScreenCTM().inverse());

            return value;

        } catch (e) {
            console.error(e);
            return { x: 0, y: 0 }
        }
    }

    return {
        transform: F2(transform),
    }

}();
