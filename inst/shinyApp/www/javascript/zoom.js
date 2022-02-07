$(document).ready(function(){
  var instance;
  var myinterval = setInterval(function(){
    var element = document.getElementById("proteinImageNoComparison");
    if(element !== null){
      clearInterval(myinterval);
      instance = panzoom(element);
    }
  }, 2000);
  var z = 1;

  $("#zoomout").on("click", function(){
    z *= 0.9;
    panzoom.zoom(z, { animate: true });
  });
  $("#zoomin").on("click", function(){
    z *= 1.1;
    panzoom.zoom(z, { animate: true });
  });
  $("#reset").on("click", function(){
    z = 1;
    panzoom.reset();
  });

});
