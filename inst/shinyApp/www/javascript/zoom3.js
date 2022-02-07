$(document).ready(function(){
  var instance;
  var myinterval = setInterval(function(){
  var element = document.getElementById("proteinImageComparisonOne");
    if(element !== null){
      clearInterval(myinterval);
      instance = panzoom(element);
      console.log("Image found")
    }
  }, 100);
  var z = 1;
  $("body").on("click", "#zoomout", function(){
    instance.smoothZoom(0, 0, 0.7);
    z *= 0.9;
  });
  $("body").on("click", "#zoomin", function(){
    instance.smoothZoom(0, 0, 1.1);
    z *= 1.1;
  });
  $("body").on("click", "#reset", function(){
    instance.smoothZoom(0, 0, 1/z);
    z = 1;
  });
  $("body").on("dblclick", "#zoomout", function(){
    return false;
  });
  $("body").on("dblclick", "#zoomin", function(){
    return false;
  });
});
