App.Modules = App.Modules || {};
App.Modules.Like = function () {

   var o = {};

   var toggleLike = function(data) {

      AjaxRoute.as("post")
         .to(App.routes.likes, {
            "message_id": data.eventElement.closest('.js-blurb').data('message-id'),
            "channel": $("body").data('channel'),
         })
         .on({
            complete: toggleLikeButton
         }, this, data);


   };

   var toggleLikeButton = function(data) {
      data.eventElement
          .toggleClass("blurb__like-button")
          .toggleClass("blurb__like-button--liked")
          .find("span")
          .toggleClass("fa-heart-o")
          .toggleClass("fa-heart");
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("click", ".js-like").to(toggleLike, this);

         return this;
      }
   };

}();
