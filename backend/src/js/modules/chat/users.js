App.Modules = App.Modules || {};
App.Modules.Users = function () {

  var o = { };

   var generateUserList = function(data) {
      $('.js-userlist-output').html(Handlebars.templates.userList(App.data.allMembers));
   };

   var displayUserCount = function(data) {
      //$(".js-user-count").html(_.keys(App.data.activeMembers).length);
   };

   var displayProfileImage = function(data) {
     $(".js-avatar").attr('src', App.data.thisMember.profileImageUrl);
   };

   var updateUserList = function(data) {
      // if (!_.has(App.data.activeMembers, data.user.user_id)) {
      //    App.data.activeMembers = _.extend(App.data.activeMembers, generateUserCollection(data.user));
      //    $('.js-chat-output').append(Handlebars.templates.userJoinedChat(App.data.activeMembers[data.user.user_id]));
      // }
   };

   return {
      init: function() { return this; },
      events: function() {
         //Events.subscribe('tl/chat/start', sanitizeUsers);

         Events.subscribe("tl/chat/payload/ready", generateUserList);
         Events.subscribe("tl/chat/payload/ready", displayUserCount);
         Events.subscribe("tl/chat/payload/ready", displayProfileImage);

         // Events.subscribe("tl/chat/users/updated", generateUserList);
         // Events.subscribe("tl/chat/users/updated", displayUserCount);

         //Events.subscribe("tl/chat/messages/presence_change", updateUserList);
         return this;
      }
   };

}();
