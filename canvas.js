$(document).ready(function(){
    var draw=false,
    canvas=document.getElementById("can"),
    ctx=canvas.getContext("2d");
    ctx.strokeStyle='red';
    ctx.fillStyle = "rgba(0, 0, 200, 0.5)";
    ctx.fillRect (30, 30, 55, 50);
    ctx.lineWidth=1;

    $("#can").mousedown(function(){draw=true;});
    $("#can").mouseup(function(){draw=false;});
    $("#can").mousemove(function(e){
	if(draw==true){
	    ctx.lineCap="round";
	    ctx.beginPath();
	    var x=e.offsetX, y=e.offsetY;
	    ctx.moveTo(x,y);
	    ctx.lineTo(x+1,y+1);
	    ctx.stroke();
	}
	});
    
    setInterval(
	function(){
	    $.getJSON('http://localhost:8080/ajax.json',
		      function(json) {
			  ctx.clearRect(0, 0, canvas.width, canvas.height);
			  ctx.beginPath();
			  ctx.moveTo(0,0);
			  ctx.lineTo(json.x,json.y);
			  ctx.stroke();
		      })},
	100);
    
});
