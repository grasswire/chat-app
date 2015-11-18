App.Modules = App.Modules || {};
App.Modules.CreateChatroom = function () {

   var o = { };

   var createNewRoom = function() {
      AjaxRoute.as("post")
         .to(App.routes.newChat, {
            title: $('.js-create-title').val(),
            description: $('.js-create-description').val()
         })
         .on({
            complete: finished
         });
   };

   var finished = function(response) {
      console.log(response);
      Events.publish("tl/modal/closing", {
         modal: $(".js-modal[data-modal-open=true]")
      });

   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("submit", ".js-create").to(createNewRoom, this);
         return this;
      }
   };

}();
