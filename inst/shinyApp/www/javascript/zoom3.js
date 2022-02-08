$(document).ready(function(){
  var instance;
  var myinterval = setInterval(function(){
    var element = document.getElementById("proteinImageComparisonTwo");
    if(element !== null){
      clearInterval(myinterval);
      instance = panzoom(element);
      console.log("Image found")
    }
  }, 300);

});
