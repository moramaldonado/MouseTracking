/**
 * Created by moramaldonado on 11/09/16.
 */
function Template(templates, elements, data, callback, finished) {
    var fileReaderCount = 0,
        fillTemplatePointer = -1,
        templates, elements, experiment;

    readFileIntoVariable(data, "experiment");
    readFileIntoVariable(elements, "elements");
    readFileIntoVariable(templates, "templates");

    function doneReading() {
        if (fileReaderCount++ == 2) {
            experiment = window["experiment"];
            elements = window["elements"];
            templates = window["templates"];
            callback();
        }
    };

    //reader
    function readFileIntoVariable(file, variable){
        var xmlhttp = getHTTPObject();
        xmlhttp.onreadystatechange = (function(file, variable){
            if (xmlhttp.readyState==4 && xmlhttp.status==200){
                window[variable] = JSON.parse((xmlhttp.responseText).replace(/[\r\n\t]+/g,""));
                doneReading();
            } else if(xmlhttp.readyState==4){
                readFileIntoVariable(file, variable);
            }
        }).bind(undefined, file, variable);
        xmlhttp.open("GET",file,true);
        xmlhttp.send();
    }

    //utils
    function stringReplaceAll(string, find, replace){
        return string.replace(new RegExp(find.replace(/([.*+?^=!:${}()|\[\]\/\\])/g, "\\$1"), 'g'), replace);
    }

    //logic
    this.fillTemplate = function(i) {
        //practice
    //    if (i==4 && exp_pointer < 3)
      //         i=3;
        //experiment
//         if(i==6 && exp_pointer < order.length-1)
  //       i = 5;
        //calibration

        if(i==3 && exp_pointer < 5)
           i=2;
        if (i==5 && exp_pointer < order_calibration.length-1)
            i=4;
         fillTemplatePointer = i;
         console.log(i);


        if (i == experiment.length-3) finished();
        return fillTemplate((experiment[i].template?templates[experiment[i].template]:templates.default), elements, experiment[i]);
    }
    this.fillNextTemplate = function() {
        return this.fillTemplate(++fillTemplatePointer);
    }
    this.fillPrevTemplate = function() {
        return this.fillTemplate(--fillTemplatePointer);
    }
    this.fillLastTemplate = function() {
        return this.fillTemplate(experiment.length-1);
    }

    function fillTemplate(template, elements, data) {
        if (!template) return "";
        var completed = template,
            sections = template.match(/\{.+?\}/g);

        if (sections){
            for (var i = 0, l = sections.length; i < l; i++) {
                var section_name = sections[i].substring(1, sections[i].length - 1);
                if(section_name==="progress") completed = stringReplaceAll(completed, sections[i], data[section_name] ? data[section_name] : fillTemplatePointer);
                else if(section_name==="nb_of_steps") completed = stringReplaceAll(completed, sections[i], data[section_name] ? data[section_name] : experiment.length);
                else completed = stringReplaceAll(completed, sections[i], fillTemplate((data[section_name] ? data[section_name] : elements[section_name]), elements, data));
            };
        }
        return completed;
    }
}

function prev_page() {
    document.body.innerHTML = myTemplate.fillPrevTemplate();
    setup_new_page();
}

function last_page(php_generated_id) {
    document.body.innerHTML = myTemplate.fillLastTemplate();
    document.getElementById('mturkFinalId').innerHTML = php_generated_id;
    setup_new_page();
}

function next_page(n, data){
    if(n !== null && typeof n === 'object'){
        data = n;
        n = undefined;
    }
    if(data){
        all_data.push(data);
        console.log(data);
    }

    document.body.innerHTML = "";
    window.setTimeout((function (n) {
        if(n)
            document.body.innerHTML = myTemplate.fillTemplate(parseInt(n));
        else
            document.body.innerHTML = myTemplate.fillNextTemplate();
        setup_new_page();
    }).bind(undefined, n), ((data && data.pause)?(Math.random()+1)*250:0));
    console.log(n)
}

//debug
var all_data = [];
var myTemplate = new Template('./templates.json', './elements.json', './experiment.json', loaded, finished);

function loaded() {
    //setup();
    window.setTimeout(function () {
        next_page();
    }, 1000);
}

function finished () {
    console.log("finished");
    runAjax(JSON.stringify(all_data))
}

function runAjax(JSONstring) {
    var params = "data=" + encodeURIComponent(JSONstring),
        ajax = getHTTPObject();

    ajax.open("POST", "new_data.php", true);
    ajax.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    ajax.onreadystatechange = function () {
        if (ajax.readyState==4 && ajax.status==200) {
            last_page(ajax.responseText);
        } else if(ajax.readyState==4 && ajax.status!=200){
            next_page();
        } else {
            console.log("readystate:"+ajax.readystate+", status:"+ajax.status);
        }
    };
    ajax.send(params);
}

function getHTTPObject() {
    if (window.XMLHttpRequest) return new XMLHttpRequest();
    else return new ActiveXObject("Microsoft.XMLHTTP");
}

