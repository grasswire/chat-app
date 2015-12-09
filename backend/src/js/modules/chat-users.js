App.Modules = App.Modules || {};
App.Modules.ChatUsers = function () {

  var o = { };

   var getChatroomList = function() {
      AjaxRoute.as("get")
         .to(App.routes.rtmStartUrl, {})
         .on({
            success: sanitizeUsers
         });
   }

   var sanitizeUsers = function(response) {
      App.data.users = _.extend(mapUsers(response.users), mapUsers([response.self]));
      Events.publish('tl/chat/users/init', {});
   };

   var generateUserList = function(data) {
      $('.js-userlist-output').html(Handlebars.templates.userList(App.data.users));
   };

   var displayUserCount = function(data) {
      $(".js-user-count").html("("+_.keys(App.data.users).length+")");
   };

   var mapUsers = function(userList) {
      var mapped = _.object(
         _.map(
            _.map(userList, function(u) {
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

      return mapped;
   };

   var updateUserList = function(data) {
      AjaxRoute.as("get")
         .to(App.routes.rtmStartUrl, {})
         .on({
            success: updateUsers
         });
   };

   var updateUsers = function(response) {
      App.data.users = _.extend(mapUsers(response.users), mapUsers([response.self]));
      Events.publish('tl/chat/users/updated', {});
   };
   return {
      init: function() { return this; },
      events: function() {
         Events.bind("load").where('body[class]', 'chatroom').to(getChatroomList, this);

         Events.subscribe("tl/chat/users/init", generateUserList);
         Events.subscribe("tl/chat/users/init", displayUserCount);
         Events.subscribe("tl/chat/users/updated", generateUserList);
         Events.subscribe("tl/chat/users/updated", displayUserCount);
         Events.subscribe("tl/chat/messages/presence_change", updateUserList);

         return this;
      }
   };

}();
