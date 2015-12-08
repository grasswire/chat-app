App.Modules = App.Modules || {};
App.Modules.ChatUsers = function () {

  var o = {
     users: {}
  };

   var getChattingUsers = function() {
      AjaxRoute.as("get")
         .to(App.routes.rtmStartUrl, {})
         .on({
            complete: mapChatUsers
         });
   }

   var mapChatUsers = function(response) {
      o.users = _.object(
         _.map(
            _.map(response.users, function(u) {
               return {
                  id: u.user_id,
                  profileImageUrl: u.profile_image_url,
                  screenName: u.twitter_screen_name
               }
            }),
            function(user) {
               return [user.id, user]
            }
         )
      );

      Events.publish('tl/chat/usersMapped', {
         users: o.users
      });
   };

   var generateUserList = function(data) {
      $('.js-userlist-output').html(Handlebars.templates.userList(data.users));
   };

   var displayUserCount = function(data) {
      $(".js-user-count").html("("+_.keys(o.users).length+")");
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe("tl/chat/setup", getChattingUsers);
         Events.subscribe("tl/chat/usersMapped", generateUserList);
         Events.subscribe("tl/chat/usersMapped", displayUserCount);

         return this;
      }
   };

}();
