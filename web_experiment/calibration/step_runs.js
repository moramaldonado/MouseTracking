/**
 * Created by moramaldonado on 11/09/16.
 */
var extra_data;
function setup_new_page () {
    var all_items_for_a_run = document.querySelectorAll('[data-run]');
    extra_data = {};
    extra_data.click_count = 0;
    window.onclick = function () {
        extra_data.click_count++;
    };

    for (var i = all_items_for_a_run.length - 1; i >= 0; i--) {
        var run_type = all_items_for_a_run[i].getAttribute('data-run');

        switch (run_type) {


            case "experimental_design":
                extra_data.item = initialize();
                extra_data.start_time = Date.now();
                break;

            case "calibration_run":
                extra_data.item = initialize_calibration();
                extra_data.start_time = Date.now();
                break;


            case "progress":
                var progress = all_items_for_a_run[i];
                var step = progress.getAttribute('data-progress');
                var steps = progress.getAttribute('data-steps');
                var progress_bar = progress.getElementsByTagName('div')[0];
                var label = progress.getElementsByTagName('span')[0];
                var advancement = 100*step/steps;
                progress_bar.style.width = advancement+'%';
                if(advancement>50){progress_bar.className = 'left';};
                label.innerHTML = step + '/' + steps;
                break;

            case "checkbox":
                all_items_for_a_run[i].onclick = function (event) {
                    event.preventDefault();
                    next_page();
                }
                break;

            case "single_button":
                all_items_for_a_run[i].onclick = function (event) {
                    event.preventDefault();
                    next_page();
                }
                break;

            case "go_back":
                all_items_for_a_run[i].onclick = function (event) {
                    event.preventDefault();
                    prev_page();
                }
                break;

            case "textarea":
                all_items_for_a_run[i].getElementsByTagName('button')[0].onclick = (function (textarea, event) {
                    event.preventDefault();
                    next_page({"strategy": textarea.value.replace(/"/g, "''")});
                }).bind(undefined, all_items_for_a_run[i].getElementsByTagName('textarea')[0]);
                break;

            case "looper":
                var buttons = all_items_for_a_run[i].getElementsByTagName('button');
                buttons[0].onclick = function(event){
                    event.preventDefault();
                    prev_page();
                }
                buttons[1].onclick = function(event){
                    event.preventDefault();
                    next_page();
                }
                break;

            case "two_buttons":
                var buttons = all_items_for_a_run[i].getElementsByTagName('button');
                extra_data.input_type = "2 buttons";
                for (var j = buttons.length - 1; j >= 0; j--) {
                    buttons[j].onclick = function (event) {
                        event.preventDefault();
                        var value = (event.target.tagName.toLowerCase() === "button") ? event.target.value : event.target.parentElement.value;
                        next_page({
                            "timestamp": Date.now(),
                            "value": value,
                            "data": extra_data,
                            "pause": true
                        })
                    };
                };
                break;

            case "scale_with_cursor":
                var with_cursor = true;
                var from_middle = false;
                if(!extra_data.input_type) extra_data.input_type = "continuous cursor";
            case "scale_from_middle":
                if(!with_cursor) var from_middle = true;
                if(!extra_data.input_type) extra_data.input_type = "continuous middle";
            case "scale":getHTTPObject()
                var scaleDiv = all_items_for_a_run[i];
                if(!extra_data.input_type) extra_data.input_type = "continuous extreme";
                var value = window.previousPageX !== undefined ? window.previousPageX : scaleDiv.getAttribute('data-start');
                window.previousPageX = undefined;
                var labels = scaleDiv.querySelectorAll('td.label');
                var scaleDiv = scaleDiv.getElementsByTagName('div')[0];
                var scaleGauge = scaleDiv.getElementsByTagName('span')[0];
                console.log("value:"+value);
                if(from_middle){
                    if(value >= 0) scaleGauge.style.left = '50%';
                    else scaleGauge.style.left = (50+value)+'%';
                }
                if(!with_cursor) scaleGauge.style.width = Math.abs(value)+'%';
                else scaleGauge.style.left = Math.abs(value)+'%';
                var left = scaleDiv.getBoundingClientRect().left + 0;
                var size = - left + (scaleDiv.getBoundingClientRect().right - 1);
                // (!with_cursor ? scaleDiv : scaleDiv.parentElement).addEventListener('mousemove', function(event) {
                window.addEventListener('mousemove', function(event) {
                    value = (event.pageX-left)/size*100;
                    value = value>100?100:value;
                    value = value<0?0:value;
                    if(from_middle){
                        value -= 50;
                        if(value >= 0) scaleGauge.style.left = '50%';
                        else scaleGauge.style.left = (50+value)+'%';
                    }
                    if(!with_cursor) scaleGauge.style.width = Math.abs(value)+'%';
                    else scaleGauge.style.left = Math.abs(value)+'%';
                });
                // (!with_cursor ? scaleDiv : scaleDiv.parentElement).addEventListener('mouseleave', function (event) {
                // 	if(event.pageX-left<0) value = 0;
                // 	if(event.pageX-left>size) value = 100;
                // 	if(from_middle && (event.pageX-left<0 || event.pageX-left>size)){
                // 		value -= 50;
                // 		if(value >= 0) scaleGauge.style.left = '50%';
                // 		else scaleGauge.style.left = (50+value)+'%';
                // 	}
                // 	if(!with_cursor) scaleGauge.style.width = Math.abs(value)+'%';
                // 	else scaleGauge.style.left = Math.abs(value)+'%';
                // });
                // labels[0].addEventListener('mouseenter', function (event) {
                // 	if(from_middle){
                // 		value = -50;
                // 		scaleGauge.style.left = '0%';
                // 		scaleGauge.style.width = '50%';
                // 	} else {
                // 		value = 0;
                // 		if(!with_cursor) scaleGauge.style.width = '0%';
                // 		else scaleGauge.style.left = Math.abs(value)+'%';
                // 	}
                // })
                // labels[1].addEventListener('mouseenter', function (event) {
                // 	if(from_middle){
                // 		value = 50;
                // 		scaleGauge.style.left = '50%';
                // 		scaleGauge.style.width = '50%';
                // 	} else {
                // 		value = 100;
                // 		if(!with_cursor) scaleGauge.style.width = '100%';
                // 		else scaleGauge.style.left = Math.abs(value)+'%';
                // 	}
                // })
                window.onclick = function (event) {
                    event.preventDefault();
                    window.previousPageX = value;
                    next_page({
                        "timestamp": Date.now(),
                        "value": value,
                        "data": extra_data,
                        "pause": true
                    })
                };
                break;

            case "7points":
                if(!extra_data.input_type) extra_data.input_type = "7 point scale";
            case "6points":
                if(!extra_data.input_type) extra_data.input_type = "6 point scale";
            case "10points":
                if(!extra_data.input_type) extra_data.input_type = "10 point scale";
                var buttons = all_items_for_a_run[i].getElementsByTagName('button');
                for (var j = buttons.length - 1; j >= 0; j--) {
                    buttons[j].onclick = function (event) {
                        var value = (event.target.tagName.toLowerCase() === "button") ? event.target.value : event.target.parentElement.value;
                        event.preventDefault();
                        next_page({
                            "timestamp": Date.now(),
                            "value": value,
                            "data": extra_data,
                            "pause": true
                        })
                    };
                };
                break;

            case "numberbox":
                var input = all_items_for_a_run[i].getElementsByTagName('input')[0];
                extra_data.input_type = "number input";
                input.focus();
                input.addEventListener('keypress', function(event){
                    var code = (event.keyCode ? event.keyCode : event.which);
                    if(code == 13 && event.target.value != ""){
                        event.preventDefault();
                        next_page({
                            "timestamp": Date.now(),
                            "value": event.target.value,
                            "data": extra_data,
                            "pause": true
                        })
                    }
                    if( (!(code >= 48 && code <= 57)) && code != 8 )
                        event.preventDefault();
                });
                input.addEventListener('input', function(){
                    this.value = this.value.match(/\d*\.?\d+/);
                });
                break;

            case "demographic":
                var agebox = all_items_for_a_run[i].querySelector('#age');

                agebox.addEventListener('keypress', function(event){
                    var code = (event.keyCode ? event.keyCode : event.which);
                    if( (!(code >= 48 && code <= 57)) && code != 8 )
                        event.preventDefault();
                });
                agebox.addEventListener('input', function(){
                    this.value = this.value.match(/\d*\.?\d+/);
                });

                var form = all_items_for_a_run[i].getElementsByTagName('table')[0];
                all_items_for_a_run[i].getElementsByTagName('button')[0].onclick = (function (event) {
                    var age = form.querySelector('#age').value;
                    var change = form.querySelector('#change').value;
                    var color = form.querySelector('#color').value;
                    var lang = form.querySelector('#lang').value;
                    var radios = form.querySelectorAll("input[type=radio]:checked");
                    var checkboxes = form.querySelectorAll("input[type=checkbox]:checked");
                    event.preventDefault();
                    if(radios.length<2 || age == "" || lang == "" || checkboxes.length<1){
                        alert("Please fill all the fields.");
                    } else {
                        console.log(checkboxes);
                        var check_list = Array.prototype.map.call(checkboxes, function(item) { return item.value; });
                        next_page({
                            "age": age,
                            "gender": radios[0].value,
                            "language": lang,
                            "handedness": radios[1].value,
                            "clicker": check_list,
                            "colorblind": color,
                            "change": change,
                            "normalized_button_size": button_size,
                            "timestamp": Date.now(),
                            "mobile": /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent),
                            "touch": 'ontouchstart' in window || 'onmsgesturechange' in window,
                            "portrait": window.innerWidth < window.innerHeight,
                            "userAgent": navigator.userAgent,
                            "windowWidth": window.innerWidth
                        });
                    }
                }).bind(undefined);
                break;

            default:
                break;
        }
    };

    window.scrollTo(0,0);
}