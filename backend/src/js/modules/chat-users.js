App.Modules = App.Modules || {};
App.Modules.ChatUsers = function () {

  var o = { };

   var getChattingUsers = function() {
      AjaxRoute.as("get")
         .to(App.routes.rtmStartUrl, {})
         .on({
            complete: mapChatUsers
         });
   }

   var mapChatUsers = function(response) {
      var users = response.users;
      console.log("Users: "+users);
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe("tl/chat/setup", getChattingUsers);

         return this;
      }
   };

}();
