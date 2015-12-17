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

   var getChannelLikes = function(data) {
      AjaxRoute.as("get")
         .to(App.routes.channelLikes + "?channel="+$('body').data('channel'))
         .on({
            complete: displayChannelLikes
         });
   };

   var displayChannelLikes = function(response) {
      var likeList = mergeUsers(response.responseArray, App.data.channelMembers);

      if (!_.isUndefined(response.responseArray)) {
         $(".js-room-likes").append(Handlebars.templates.channelLike(likeList));
      }
   };

   var mergeUsers = function(list, users) {

      return _.each(list, function(item) {
         item.user = App.data.channelMembers[item.user];
      });
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
         Events.subscribe("tl/chat/users/init", getChannelLikes);
         Events.bind("click", ".js-like").to(toggleLike, this);

         return this;
      }
   };

}();
