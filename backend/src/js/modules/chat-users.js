App.Modules = App.Modules || {};
App.Modules.ChatUsers = function () {

  var o = {
     users: []
  };

   var getChattingUsers = function() {
      AjaxRoute.as("get")
         .to(App.routes.rtmStartUrl, {})
         .on({
            complete: mapChatUsers
         });
   }

   var mapChatUsers = function(response) {
      o.users = _.map(response.users, function(user) {
         return {
            profileImageUrl: user.profileImageUrl,
            screenName: user.twitterScreenName,
         }
      });

      Events.publish('tl/chat/usersMapped', {
         users: o.users
      });
   };

   var generateUserList = function(data) {
      $('.js-userlist-output').html(Handlebars.templates.userList(data.users));
   };

   var displayUsers = function(data) {
      console.log("click");
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe("tl/chat/setup", getChattingUsers);
         Events.subscribe("tl/chat/usersMapped", generateUserList);

         Events.bind(".js-toggle-users").to(displayUsers, this);
         return this;
      }
   };

}();
