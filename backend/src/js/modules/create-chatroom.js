App.Modules = App.Modules || {};
App.Modules.CreateChatroom = function () {

   var o = { };

   var createNewRoom = function() {
      AjaxRoute.as("post")
         .to(App.routes.newChat, {
            title: $('.js-create-title-input').val(),
            topic: $('.js-create-description-input').val(),
            color: $('.js-create-color.js-color-selected').data('color').toString()
         })
         .on({
            success: redirectToRoom,
            fail: function(response) {
               Events.publish("tl/errors", {error: response});
            }
         });

         return false;
   };
   var redirectToRoom = function(response) {
      $('.js-create-title-input').val("");
      $('.js-create-description-input').val("");
      o.validator.resetForm();
      o.validator.reset();

      if (response.slug == "undefined" || _.isUndefined(response.slug)) {
         location.href = "/";
         return false;
      }

      location.href = '/room/'+response.slug;
   };

   return {
      init: function() {
         o.validator = $(".js-create").validate({
            onkeyup: false,
            onfocusout: false,
            rules: {
               "create-title": {required: true},
               "create-description": {required: true}
            },
            messages: {
               "create-title": {
                  required: "Every chat room needs a title"
               },
               "create-description": {
                  required: "Every chat room needs a description"
               }
            }
         });
         return this;
      },
      events: function() {
         Events.bind("submit", ".js-create").to(createNewRoom, this);
         return this;
      }
   };

}();
