/**
 * Created by moramaldonado on 13/01/2017.
 */
var ctx;
var zero_point, lessone_point, plusone_point, button_size = false;
var temp_button_rect;
var calibration_type;
var order_calibration = shuffle(['RG1', 'RG2', 'RG3','GR1', 'GR2', 'GR3', 'GG1', 'RR1'])

function initialize_calibration(){
    mouse_log = [];
    exp_pointer++;

    document.querySelector('[data-run=calibration_run]').className = "invisible";
    zero_point = document.getElementById('start').getBoundingClientRect();
    zero_point = {
        'x': zero_point.left + zero_point.width/2,
        'y': zero_point.top
    };
    lessone_point = document.getElementById('false').getBoundingClientRect();
    lessone_point = {
        'x': lessone_point.right,
        'y': lessone_point.bottom
    }
    plusone_point = document.getElementById('true').getBoundingClientRect();
    plusone_point = {
        'x': plusone_point.left,
        'y': plusone_point.bottom
    }

    temp_button_rect = document.getElementById('true').getBoundingClientRect();
    button_size = {};
    button_size.x = temp_button_rect.width / (zero_point.x - lessone_point.x);
    button_size.y = temp_button_rect.height / (zero_point.y - lessone_point.y);


    var c = document.getElementById('points');
    c.setAttribute("width", window.innerHeight);
    c.setAttribute("height", window.innerHeight);
    ctx = c.getContext("2d");
    //var rect = ctx.getBoundingClientRect();
    var width = c.width;
    var height = c.height;
    var start_size = document.getElementById('start').getBoundingClientRect();

    var x1 = width/2;
    var y1 = height-(start_size.height*1.5);
    var button_right = width-(temp_button_rect.width/3);
    var button_left =  temp_button_rect.width/3;
    var y2 = temp_button_rect.height;
    var t1 = 400;
    var t21  = 800;
    var t22  = 1000;
    var t23  = 1200;


    console.log(start_size.height, start_size.width, width, height, x1, y1)

    // on click bindings
    started_tracking = false;
    document.getElementById('start').addEventListener('click', function () {
        if (!started_tracking) {
            extra_data.start_track = Date.now();

            // log mouse first click
            var norm_x = (event.clientX - zero_point.x) / (zero_point.x - lessone_point.x)
            var norm_y = (zero_point.y - event.clientY) / (zero_point.y - lessone_point.y)
            mouse_log.push([norm_x, norm_y, extra_data.start_track]);

            started_tracking = true;
            document.querySelector('[data-run=calibration_run]').className = "visible";
            console.log(button_size);

  /*          // button right
            ctx.fillStyle = "red";
            ctx.strokeStyle ='black';
            ctx.lineWidth = 4;

            ctx.beginPath();
            ctx.arc(button_right, y2, 20, 0, 2 * Math.PI);
            ctx.stroke();
            ctx.fill();
            ctx.closePath();

            // button left
            ctx.fillStyle = "blue";
            ctx.strokeStyle ='black';
            ctx.lineWidth = 4;

            ctx.beginPath();
            ctx.arc(button_left, y2, 20, 0, 2 * Math.PI);
            ctx.fill();
            ctx.stroke();
            ctx.closePath();
*/

            switch (order_calibration[exp_pointer].substring(0,3)) {
                case 'RG1':
                    setTimeout(function() {setColor('red');}, t1);
                    setTimeout(function() {setColor('blue');}, t21);
                    console.log(exp_pointer);
                    break;

                case 'RG2':
                    setTimeout(function() {setColor('red');}, t1);
                    setTimeout(function() {setColor('blue');}, t22);
                    console.log(exp_pointer);
                    break;

                case 'RG3':
                    setTimeout(function() {setColor('red');}, t1);
                    setTimeout(function() {setColor('blue');}, t23);
                    console.log(exp_pointer);
                    break;

                case 'GR1':
                    setTimeout(function() {setColor('blue');}, t1);
                    setTimeout(function() {setColor('red');}, t21);
                    console.log(exp_pointer);
                    break;

                case 'GR2':
                    setTimeout(function() {setColor('blue');}, t1);
                    setTimeout(function() {setColor('red');}, t22);
                    console.log(exp_pointer);
                    break;

                case 'GR3':
                    setTimeout(function() {setColor('blue');}, t1);
                    setTimeout(function() {setColor('red');}, t23);
                    console.log(exp_pointer);
                    break;

                case 'GG1':
                    setTimeout(function() {setColor('blue');}, t1);
                    console.log(exp_pointer);
                    break;

                case 'RR1':
                    setTimeout(function() {setColor('red');}, t1);
                    console.log(exp_pointer);
                    break;

            }





        }

        //extra_data.design = setup2;
        document.body.addEventListener('mousemove', record_mouse);
        extra_data.tooslow = false;
        moved_before_timeout = setTimeout(function () {
            console.log('didnt move');
            moved_before_400ms = false;
        }, 400)});

    function logger_stop_button (event) {
        if(started_tracking){
            clearTimeout(response_timeout);

            clearTimeout(moved_before_timeout);

            document.body.removeEventListener('mousemove', record_mouse)
            console.log(mouse_log);
            setColor('white');
            // next
            extra_data.end_track = Date.now();
            // log mouse first click
            var norm_x = (event.clientX - zero_point.x) / (zero_point.x - lessone_point.x)
            var norm_y = (zero_point.y - event.clientY) / (zero_point.y - lessone_point.y)
            mouse_log.push([norm_x, norm_y, extra_data.end_track]);
            next_page({
                mouse_log: mouse_log,
                "timestamp": Date.now(),
                "value": event?event.target.id:"--",
                "data": extra_data
            });}}
    document.getElementById('false').addEventListener('click', logger_stop_button)
    document.getElementById('true').addEventListener('click', logger_stop_button)
    var report_type;
    report_type = "calibration";
    console.log(report_type, calibration_type);
    return {"item_number": exp_pointer, "type": report_type, "raw": calibration_type};


    function setColor(color)
        {  var str = 'Click on the ' + "\"" +color + "\"" + ' button.'
            document.getElementsByTagName('p')[0].innerHTML = str.fontcolor(color) ;
        }





}
