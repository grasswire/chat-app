App.Modules = App.Modules || {};
App.Modules.ChatWebSocketConnection = function () {

   var o = { };

   var setup = function() {
      var url = App.routes.chatRoom;
      App.socket = new WebSocket(url.replace("http:", "ws:").replace("https:", "wss:").replace("?", "").replace("#", ""));
      Events.publish("tl/chat/socket/ready", {});
   }

   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe("tl/chat/users/init", setup);

         return this;
      }
   };

}();
