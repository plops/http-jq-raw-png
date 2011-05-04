$(document).ready(function(){
    var draw=false,
    canvas=document.getElementById("can"),
    ctx=canvas.getContext("2d");
    ctx.strokeStyle='red';
    ctx.fillStyle = "rgba(0, 0, 200, 0.5)";
    ctx.fillRect (30, 30, 55, 50);
    
    $("#can").mousedown(function(){draw=true;});
    $("#can").mouseup(function(){draw=false;});
    $("#can").mousemove(function(e){
	if(draw==true){
	    ctx.lineWidth=14;
	    ctx.lineCap="round";
	    ctx.beginPath();
	    var x=e.offsetX, y=e.offsetY;
	    ctx.moveTo(x,y);
	    ctx.lineTo(x+1,y+1);
	    ctx.stroke();
	}
	});
    $("#eraser").click(function(){
	ctx.strokeStyle='#fff';
    });
    
    $.get('http://localhost:8080/ajax/test.html', function(data) {
	$('.result').html(data);
	alert('Load was performed.');
    });
});