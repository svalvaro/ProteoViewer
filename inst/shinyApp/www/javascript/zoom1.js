$(document).ready(function(){
  var instance;
  var myinterval = setInterval(function(){
    var element = document.getElementById("proteinImageNoComparison");
    if(element !== null){
      clearInterval(myinterval);
      instance = panzoom(element);
    }
  }, 100);

});
