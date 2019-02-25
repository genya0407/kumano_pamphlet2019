(function() {
    const max = 7;
    const interval = 5000;
    var cnt = 0;
    setInterval(function() {
        cnt = (cnt + 1) % max;
        const image = $("img[src^='slide']");
        image.fadeOut('slow', function() {
            image.attr('src', 'slide' + cnt + '.JPG');
            image.fadeIn('slow');
        })
    }, interval);
})()