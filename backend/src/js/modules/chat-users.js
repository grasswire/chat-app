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
      console.log(response);
      App.data.channelMembers = App.Helpers.mapUsers(response.members);
      App.data.activeMembers = _.extend(App.Helpers.mapUsers(response.users), App.Helpers.mapUsers([response.self]));

      Events.publish('tl/chat/users/init', {
         currentUser: response.self
      });
   };

   var generateUserList = function(data) {
      $('.js-userlist-output').html(Handlebars.templates.userList(App.data.channelMembers));
   };

   var displayUserCount = function(data) {
      $(".js-user-count").html(_.keys(App.data.activeMembers).length);
   };

   var displayProfileImage = function(data) {
     $(".js-avatar").attr('src', App.data.activeMembers[data.currentUser.user_id].profileImageUrl);
   };

   var updateUserList = function(data) {
      if (!_.has(App.data.activeMembers, data.user.user_id)) {
         App.data.activeMembers = _.extend(App.data.activeMembers, App.Helpers.mapUsers([data.user]));
         $('.js-chat-output').append(Handlebars.templates.userJoinedChat(App.data.activeMembers[data.user.user_id]));
      }
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("load").where('body[class]', 'chatroom').to(getChatroomList, this);

         Events.subscribe("tl/chat/users/init", generateUserList);
         Events.subscribe("tl/chat/users/init", displayUserCount);
         Events.subscribe("tl/chat/users/init", displayProfileImage);

         Events.subscribe("tl/chat/users/updated", generateUserList);
         Events.subscribe("tl/chat/users/updated", displayUserCount);

         Events.subscribe("tl/chat/messages/presence_change", updateUserList);
         return this;
      }
   };

}();
