App.Modules = App.Modules || {};
App.Modules.Errors = function () {

   var displayError = function(response) {
      $(".js-message").show();
      $(".js-message-text").html("Sorry we've run into an issue while trying to create your room");
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe('tl/errors', displayError);

         return this;
      }
   };

}();
