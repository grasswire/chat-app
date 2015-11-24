App.Modules = App.Modules || {};
App.Modules.CreateChatroom = function () {

   var o = { };

   var createNewRoom = function() {
      AjaxRoute.as("post")
         .to(App.routes.newChat, {
            title: $('.js-create-title-input').val(),
            description: $('.js-create-description-input').val()
         })
         .on({
            complete: finished
         });

         return false;
   };

   var finished = function(response) {
      location.href = '/room/'+response.slug;
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("submit", ".js-create").to(createNewRoom, this);
         return this;
      }
   };

}();
