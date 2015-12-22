App.Modules = App.Modules || {};
App.Modules.Errors = function () {

   var displayError = function(response) {
      $(".js-message").show();
      $(".js-message-text").html("Sorry we've run into an issue while trying to create your room");
   };

   var closeMessage = function() {
      $(".js-message").hide();
      $(".js-message-text").html("check-signin.js");
   }
   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe('tl/errors', displayError);
         Events.bind("key", [27]).to(closeMessage, window);
         return this;
      }
   };

}();
