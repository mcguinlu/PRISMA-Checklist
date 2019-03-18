Shiny.addCustomMessageHandler("toggleSectionIcon",
  function(message) {
    var icon = $(message.id);
    var complete = message.val;

    if(complete === true){
      icon[0].className = icon[0].className.replace('fa-eye', 'fa-check');
      icon[0].className = icon[0].className.replace('fa-exclamation-circle', 'fa-check');
      icon[0].style = "color:inherit";

    } else if(complete === false){
      icon[0].className = icon[0].className.replace('fa-eye', 'fa-exclamation-circle');
      icon[0].className = icon[0].className.replace('fa-check', 'fa-exclamation-circle');
      icon[0].style = "color:#d9534f";
    } else {
      icon[0].className = icon[0].className.replace('fa-check', 'fa-eye');
      icon[0].className = icon[0].className.replace('fa-exclamation-circle', 'fa-eye');
      icon[0].style = "color:inherit";
    }  
    
  }
);