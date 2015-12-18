App.Modules = App.Modules || {};
App.Modules.CreateChatroom = function () {

   var o = { };

   var createNewRoom = function() {
      AjaxRoute.as("post")
         .to(App.routes.newChat, {
            title: $('.js-create-title-input').val(),
            topic: $('.js-create-description-input').val(),
            color: $('.js-create-color.js-color-selected').data('color')
         })
         .on({
            complete: redirectToRoom
         });

         return false;
   };

   var redirectToRoom = function(response) {
      $('.js-create-title-input').val("");
      $('.js-create-description-input').val("");

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
