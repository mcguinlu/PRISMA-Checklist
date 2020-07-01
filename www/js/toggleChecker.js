Shiny.addCustomMessageHandler("toggleChecker",
  function(message) {
    var el = document.getElementById(message.id);
    var eldiv = document.getElementById(message.divId);
    
    if(message.val === null || message.val.length === 0){
      $(el).removeClass('fa fa-check').addClass('fa fa-exclamation-circle');
      $(eldiv).title = "This question needs to be answered.";
    } else if(message.val !== null){
      $(el).removeClass('fa fa-exclamation-circle').addClass('fa fa-check');
      document.getElementById(message.divId).removeAttribute('title');
      document.getElementById(message.divId).style = "color:black";
    }
  }
);