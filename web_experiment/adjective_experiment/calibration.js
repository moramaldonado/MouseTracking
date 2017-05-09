/**
 * Created by moramaldonado on 13/01/2017.
 */
var ctx;
var zero_point, lessone_point, plusone_point, button_size = false;
var temp_button_rect;
var calibration_type;

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

    if(!button_size){
        temp_button_rect = document.getElementById('true').getBoundingClientRect();
        button_size = {};
        button_size.x = temp_button_rect.width / (zero_point.x - lessone_point.x)
        button_size.y = temp_button_rect.height / (zero_point.y - lessone_point.y)
    }

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

            switch (exp_pointer) {

                //Straight
                case 7:
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, y1, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(button_right, y2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_right, y2, 12, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    console.log(exp_pointer);
                    break;

                //Straight
                case 6:
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, y1, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(button_left, y2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_left, y2, 12, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    console.log(exp_pointer);

                    break;

                case 8:
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, y1, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();


                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(button_right, y2)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_right, y2, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(button_right, y2)
                    ctx.lineTo(button_left, y2)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();


                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_left, y2, 12, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();
                    calibration_type = 'deviation_right';
                    console.log(exp_pointer);
                    break;

                case 9:
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, y1, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();


                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(button_left, y2)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();


                    ctx.beginPath();
                    ctx.moveTo(button_right, y2)
                    ctx.lineTo(button_left, y2)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_left, y2, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_right, y2, 12, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    console.log(exp_pointer);
                    break;

                case 4:
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, y1, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(button_right - width/4, height/2)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_right - width/4, height/2, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(button_left + width/4, height/4);
                    ctx.lineTo(button_right - width/4, height/2)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_left + width/4, height/4, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(button_right, y2)
                    ctx.lineTo(button_left + width/4, height/4)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_right, y2, 12, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    console.log(exp_pointer);
                    break;


                case 5:
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, y1, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();


                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(button_left + width/4, height/2)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_left + width/4, height/2, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(button_left + width/4, height/2);
                    ctx.lineTo(button_right - width/4, height/4)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_right - width/4, height/4, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(button_left, y2)
                    ctx.lineTo(button_right - width/4, height/4)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_left, y2, 12, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    //ctx.font = "20px Georgia";
                    //ctx.fillText("Click", button_left, y2/2);


                    console.log(exp_pointer);
                    break;

                case 1:
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, y1, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(button_right-width/7, height/3)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_right-width/7, height/3, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(button_right-width/7, height/3)
                    ctx.lineTo(button_left, y2)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_left, y2, 12, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();
                    calibration_type = 'deviation_right';
                    console.log(exp_pointer);
                    break;

                case 0:
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, y1, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(button_left+width/7, height/3)
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_left+width/7, height/3, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(button_left+width/7, height/3);
                    ctx.lineTo(button_right, y2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_right, y2, 12, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();
                    calibration_type = 'deviation_right';
                    console.log(exp_pointer);
                    break;

                case 3:
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, y1, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(x1, height/2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, height/2, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(x1, height/2);
                    ctx.lineTo(button_left+width/8, y2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";

                    ctx.stroke();
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_left+width/8, y2, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(button_left+width/8, y2);
                    ctx.lineTo(button_right, y2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_right, y2, 12, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();
                    calibration_type = 'deviation_right';
                    console.log(exp_pointer);
                    break;

                case 2:
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, y1, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(x1, height/2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, height/2, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(x1, height/2);
                    ctx.lineTo(button_right-width/8, y2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";

                    ctx.stroke();
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_right-width/8, y2, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(button_right-width/8, y2);
                    ctx.lineTo(button_left, y2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_left, y2, 12, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();
                    calibration_type = 'deviation_right';
                    console.log(exp_pointer);
                    break;


                case 10:
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, y1, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(x1, height/2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, height/2, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();



                    ctx.beginPath();
                    ctx.moveTo(x1, height/2);
                    ctx.lineTo(button_right, y2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();


                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_right, y2, 12, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();
                    calibration_type = 'deviation_right';
                    console.log(exp_pointer);
                    break;

                case 11:
                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, y1, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    ctx.beginPath();
                    ctx.moveTo(x1, y1);
                    ctx.lineTo(x1, height/2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();

                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(x1, height/2, 5, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();


                    ctx.beginPath();
                    ctx.moveTo(x1, height/2);
                    ctx.lineTo(button_left, y2);
                    ctx.closePath();
                    ctx.strokeStyle = "red";
                    ctx.stroke();


                    ctx.fillStyle = "red";
                    ctx.beginPath();
                    ctx.arc(button_left, y2, 12, 0, 2 * Math.PI);
                    ctx.closePath();
                    ctx.fill();

                    calibration_type = 'straight_left';
                    console.log(exp_pointer);
                    break;



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

}
