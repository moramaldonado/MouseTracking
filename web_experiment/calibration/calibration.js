/**
 * Created by moramaldonado on 13/01/2017.
 */
var ctx;
var zero_point, lessone_point, plusone_point, button_size = false;
var temp_button_rect;
var calibration_type;
var straight = Array(32).fill('RR1').concat(Array(32).fill('BB1'));
//var uncertain = Array(2).fill('RR2').concat(Array(2).fill('BB2'), Array(2).fill('RR3'), Array(2).fill('BB3'));
var deviated = Array(4).fill('RB1').concat(Array(4).fill('BR1'), Array(4).fill('RB2'), Array(4).fill('BR2'), Array(4).fill('BR3'), Array(4).fill('RB3'));
//var order_calibration =  shuffle(straight.concat(deviated, uncertain));
var order_calibration =  shuffle(straight.concat(deviated));
var myreds = ['#ff4d4d', '#ff0000', '#cc0000', '#800000'];
var myblues = ['#4d4dff', '#0000ff', '#0000cc', '#000080'];
var red;
var blue;

function initialize_calibration() {
    // initializing variables
    moved_before_400ms = true;
    mouse_log = [];
    exp_pointer++;
    console.log(order_calibration);

    if (practice_mode && document.querySelector('[data-run=calibration_run]').getAttribute('data-practice') == "false") {
        exp_pointer = 0;
        practice_mode = false;
        warner = false;
    }

    // warning
    if (!warner)
        document.getElementById('warning').style.visibility = "hidden";
    else {
        if (warner === "Good!") {
            document.getElementById('warning').style.animation = "none";
            document.getElementById('warning').style.webkitAnimation = "none";
        }
        document.querySelector('#warning p').innerHTML = warner;
    }
    warner = false;
    document.querySelector('[data-run=calibration_run]').className = "invisible";
    zero_point = document.getElementById('start').getBoundingClientRect();
    zero_point = {
        'x': zero_point.left + zero_point.width / 2,
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
    c.setAttribute("width", window.innerHeight+50);
    c.setAttribute("height", window.innerHeight+10);
    ctx = c.getContext("2d");
    //var rect = ctx.getBoundingClientRect();
    var width = c.width;
    var height = c.height;
    var start_size = document.getElementById('start').getBoundingClientRect();

    var start_width= 5;
    var start_height= 5;
    var end_width= width-10;
    var end_height= height-30;
    var blue = "#4E60F0";
    var red =  '#ff0000';

    var t1 = 0;
    var ty;
    var t2 = Number(0.2, 0.7);
    var listener;

    var flag_background;


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
            flag_background = false;
            if (practice_mode) switch (exp_pointer) {
                // red, blue, red, blue
                case 0:
                    roundedRect(ctx, start_width, start_height, end_width, end_height, 10, red);
                    //roundedRect(ctx, (width/2 - 250), (height/2 - 15), 500, 40, 10, 'red');
                    //listener = change_mouse_background.bind(null, red, red, t1, t2);
                    document.body.addEventListener('mousemove', listener);
                    console.log(exp_pointer);
                    break;

                case 1:
                    roundedRect(ctx, start_width, start_height, end_width, end_height, 10, blue);
                    //roundedRect(ctx, (width/2 - 250), (height/2 - 15), 500, 40, 10, 'blue');
                    //listener = change_mouse_background.bind(null, blue, blue, t1, t2);
                    document.body.addEventListener('mousemove', listener);
                    console.log(exp_pointer);
                    break;

                case 2:
                    roundedRect(ctx, start_width, start_height, end_width, end_height, 10, blue);
                    //roundedRect(ctx, (width/2 - 250), (height/2 - 15), 500, 40, 10, 'blue');
                    listener = change_mouse_background.bind(null, blue, red, t1, .3);
                    document.body.addEventListener('mousemove', listener);
                    console.log(exp_pointer);
                    break;

                case 3:
                    roundedRect(ctx, start_width, start_height, end_width, end_height, 10, red);
                    listener = change_mouse_background.bind(null, red, blue, t1, .3);
                    document.body.addEventListener('mousemove', listener);
                    console.log(exp_pointer);
                    break;

                case 4:
                    roundedRect(ctx, start_width, start_height, end_width, end_height, 10, blue);
                    //roundedRect(ctx, (width/2 - 250), (height/2 - 15), 500, 40, 10, 'blue');
                    listener = change_mouse_background.bind(null, blue, red , t1, .8);
                    document.body.addEventListener('mousemove', listener);
                    console.log(exp_pointer);
                    break;

                case 5:
                    //roundedRect(ctx, (width/2 - 250), (height/2 - 15), 500, 40, 10, 'red');
                    roundedRect(ctx, start_width, start_height, end_width, end_height, 10, red);
                    listener = change_mouse_background.bind(null, red, blue, t1, .8);
                    document.body.addEventListener('mousemove', listener);
                    console.log(exp_pointer);
                    break;
            }

            else {
                switch (order_calibration[exp_pointer].substring(0, 3)) {

                    //deviated

                    case 'RB1':
                        //ty = Number(0.2, 0.7);
                        ty = 0.4;
                        roundedRect(ctx, start_width, start_height, end_width, end_height, 10, red);
                        listener = change_mouse_background.bind(null, red, blue, t1, ty);
                        document.body.addEventListener('mousemove', listener);
                        console.log(exp_pointer);
                        break;

                    case 'RB2':
                        //ty = Number(0.2, 0.7);
                        ty = 0.7;
                        roundedRect(ctx, start_width, start_height, end_width, end_height, 10, red);
                        listener = change_mouse_background.bind(null, red, blue, t1, ty);
                        document.body.addEventListener('mousemove', listener);
                        console.log(exp_pointer);
                        break;

                    case 'RB3':
                        ty = 0.9;
                        //ty = Number(0.4, 0.9);
                        roundedRect(ctx, start_width, start_height, end_width, end_height, 10, red);
                        listener = change_mouse_background.bind(null, red, blue, t1, ty);
                        document.body.addEventListener('mousemove', listener);
                        console.log(exp_pointer);
                        break;

                    case 'BR1':
                        ty = 0.4;
                        //ty = Number(0.2, 0.7);
                        roundedRect(ctx, start_width, start_height, end_width, end_height, 10, blue);
                        listener = change_mouse_background.bind(null, blue, red, t1, ty);
                        document.body.addEventListener('mousemove', listener);
                        console.log(exp_pointer);
                        break;

                    case 'BR2':
                        ty = 0.7;
                        //ty = Number(0.2, 0.7);
                        roundedRect(ctx, start_width, start_height, end_width, end_height, 10, blue);
                        listener = change_mouse_background.bind(null, blue, red, t1, ty);
                        document.body.addEventListener('mousemove', listener);
                        console.log(exp_pointer);
                        break;

                    case 'BR3':
                        ty = 0.9;
                        //ty = Number(0.4, 0.9);
                        roundedRect(ctx, start_width, start_height, end_width, end_height, 10, blue);
                        listener = change_mouse_background.bind(null, blue, red, t1, ty);
                        document.body.addEventListener('mousemove', listener);
                        console.log(exp_pointer);
                        break;

                    //straight
                    case 'BB1':
                        roundedRect(ctx, start_width, start_height, end_width, end_height, 10, blue);
                        console.log(exp_pointer);
                        break;

                    case 'RR1':
                        roundedRect(ctx, start_width, start_height, end_width, end_height, 10, red);
                        console.log(exp_pointer);
                        break;

                    //uncertain
/*
                    case 'BB2':
                        ty = .3;
                        roundedRect(ctx, start_width, start_height, end_width, end_height, 10, 'white');
                        listener = change_mouse_background.bind(null, 'white', blue, t1, ty);
                        document.body.addEventListener('mousemove', listener);
                        console.log(exp_pointer);
                        break;

                    case 'RR2':
                        ty = .3;
                        roundedRect(ctx, start_width, start_height, end_width, end_height, 10, 'white');
                        listener = change_mouse_background.bind(null, 'white', red, t1, ty);
                        document.body.addEventListener('mousemove', listener);
                        console.log(exp_pointer);
                        break;

                    case 'BB3':
                        ty = .45;
                        roundedRect(ctx, start_width, start_height, end_width, end_height, 10, 'white');
                        listener = change_mouse_background.bind(null, 'white', blue, t1, ty);
                        document.body.addEventListener('mousemove', listener);
                        console.log(exp_pointer);
                        break;

                    case 'RR3':
                        ty = .45;
                        roundedRect(ctx, start_width, start_height, end_width, end_height, 10, 'white');
                        listener = change_mouse_background.bind(null, 'white', red, t1, ty);
                        document.body.addEventListener('mousemove', listener);
                        console.log(exp_pointer);
                        break;
*/

                }
            }
            extra_data.ty = ty;
            document.getElementById('warning').style.visibility = "hidden";
            document.body.addEventListener('mousemove', record_mouse);
            extra_data.tooslow = false;
            response_timeout = setTimeout(function () {
                warner = "Too slow. Please answer faster.";
                extra_data.tooslow = true;
                logger_stop_button();
            }, 6500)
            moved_before_timeout = setTimeout(function () {
                console.log('didnt move');
                moved_before_400ms = false;
            }, 400)
        }
    });

    //HERE

    function logger_stop_button(event) {
        if (started_tracking) {
            clearTimeout(response_timeout);
            clearTimeout(moved_before_timeout);
            //setColor('white');

            if (event && practice_mode && moved_before_400ms && (([0, 2, 4].indexOf(exp_pointer) > -1 && event.target.id == "true") || ([1, 3, 5].indexOf(exp_pointer) > -1 && event.target.id == "false"))) {
                warner = "Good!"
            } else if (event && practice_mode && (([1, 3, 5].indexOf(exp_pointer) > -1 && event.target.id != "false"))) {
                warner = "Incorrect! When you clicked the square was red."

            } else if (event && practice_mode && (([0, 2, 4].indexOf(exp_pointer) > -1 && event.target.id != "true"))) {
                warner = "Incorrect! When you clicked the square was blue."

            } else if (event && practice_mode && !moved_before_400ms && (([0, 2, 4].indexOf(exp_pointer) > -1 && event.target.id == "true") || ([1, 3, 5].indexOf(exp_pointer) > -1 && event.target.id == "false"))) {
                warner = "Please, start moving the mouse earlier."
            }
            console.log(mouse_log);
            document.body.removeEventListener('mousemove', record_mouse);
            document.body.removeEventListener('mousemove', listener);
            // next
            extra_data.end_track = Date.now();
            // log mouse first click
            var norm_x = (event.clientX - zero_point.x) / (zero_point.x - lessone_point.x)
            var norm_y = (zero_point.y - event.clientY) / (zero_point.y - lessone_point.y)
            mouse_log.push([norm_x, norm_y, extra_data.end_track]);
            next_page({
                mouse_log: mouse_log,
                "timestamp": Date.now(),
                "value": event ? event.target.id : "--",
                "data": extra_data
            });
        }
    }

    document.getElementById('false').addEventListener('click', logger_stop_button)
    document.getElementById('true').addEventListener('click', logger_stop_button)

    var report_type;
    report_type = "calibration";
    console.log(report_type, calibration_type);


    function setColor(color) {
        document.body.style.backgroundColor = color;
    }
    function draw_square(width, height)    {
        //ctx.fillStyle = color;
        ctx.strokeStyle = 'white';
        ctx.lineWidth = 15;
        ctx.beginPath();
        //ctx.rect((width / 2) - 100, (height / 2) - 50, 200, 100);
        //ctx.rect((width / 2) - 250 , height/3, 500, 30);
        ctx.rect(5, 5, width-10, height-30);
        ctx.stroke();
        //ctx.fill();
        ctx.closePath();




    }


    function roundedRect(ctx, x, y, width, height, radius, color) {
        //ctx.fillStyle = color;
        ctx.strokeStyle = color;
        ctx.lineWidth=10;
        ctx.beginPath();
        //ctx.strokeRect(x, y, width,height);
        ctx.moveTo(x, y + radius);
        ctx.lineTo(x, y + height - radius);
        ctx.arcTo(x, y + height, x + radius, y + height, radius);
        ctx.lineTo(x + width - radius, y + height);
        ctx.arcTo(x + width, y + height, x + width, y + height-radius, radius);
        ctx.lineTo(x + width, y + radius);
        ctx.arcTo(x + width, y, x + width - radius, y, radius);
        ctx.lineTo(x + radius, y);
        ctx.arcTo(x, y, x, y + radius, radius);
        //ctx.fill();
        ctx.stroke();
        ctx.closePath();
    }
//5, 5, width-10, height-30
    function change_mouse_background(color1, color2, mt1, mt2) {
        var norm_y = (zero_point.y - event.clientY) / (zero_point.y - lessone_point.y);
        if (norm_y > mt1 && flag_background == false) {
            //roundedRect(ctx, (width/2 - 250), (height/2 - 15), 500, 40, 10, color1);
            roundedRect(ctx, 5, 5, width-10, height-30, 10, color1);
        }
        if (norm_y > mt2 && flag_background == false) {
            draw_square( width, height);
            roundedRect(ctx, 5, 5, width-10, height-30, 10, color2);
            flag_background = true;
        }

        }

    function Number(min,max){
        var i = 0;
        while (i > max | i < min) {
            i = parseFloat(Math.round((Math.random()) * 100) / 100).toFixed(2);}

        return i;}

    return {"item_number": exp_pointer, "type": report_type, "raw": practice_mode?("practice"+exp_pointer):order_calibration[exp_pointer]};


}

