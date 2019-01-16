Shiny.addCustomMessageHandler("toggleChecker",
  function(message) {
    var el = document.getElementById(message.id);
    
    if(message.val === null || message.val.length === 0){
      //alert(JSON.stringify(message));
      $(el).removeClass('fa fa-check').addClass('fa fa-exclamation-circle');
      document.getElementById(message.divId).title = "This question needs to be answered.";
    } else if(message.val !== null){
      $(el).removeClass('fa fa-exclamation-circle').addClass('fa fa-check');
      document.getElementById(message.divId).removeAttribute('title');
    }
  }
);