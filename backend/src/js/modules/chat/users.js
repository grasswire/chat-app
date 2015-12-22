App.Modules = App.Modules || {};
App.Modules.ChatUsers = function () {

  var o = { };

   var generateAllMemberCollection = function(rawList) {
      return _.object(
         Mapper.collection(
            Mapper.collection(rawList, App.Transformers.users),
               function(item) { return [item.id, item]}
            )
         );
   };

   var sanitizeUsers = function(response) {
      console.log(response.users);
      App.data.allMembers    = generateAllMemberCollection(response.users);
      App.data.thisMember    = null;

      if (! _.isNull(response.self)) {
         App.data.thisMember    = Mapper.item(response.self, App.Transformers.users);
         App.data.allMembers    = _.extend(App.data.allMembers, generateAllMemberCollection(response.self))
      }

      Events.publish('tl/chat/users/init');
   };

   var generateUserList = function(data) {
      $('.js-userlist-output').html(Handlebars.templates.userList(App.data.allMembers));
   };

   var displayUserCount = function(data) {
      $(".js-user-count").html(_.keys(App.data.activeMembers).length);
   };

   var displayProfileImage = function(data) {
     $(".js-avatar").attr('src', App.data.thisMember.profileImageUrl);
   };

   var updateUserList = function(data) {
      if (!_.has(App.data.activeMembers, data.user.user_id)) {
         App.data.activeMembers = _.extend(App.data.activeMembers, generateUserCollection(data.user));
         $('.js-chat-output').append(Handlebars.templates.userJoinedChat(App.data.activeMembers[data.user.user_id]));
      }
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe('tl/chat/start', sanitizeUsers);

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
