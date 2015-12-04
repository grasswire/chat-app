App.Modules = App.Modules || {};
App.Modules.Chat = function () {

   var o = { };

   var setup = function() {
      var url = App.routes.chatRoom;

      Events.publish("tl/chat/setup", {
         connection: new WebSocket(url.replace("http:", "ws:").replace("https:", "wss:").replace("?", "").replace("#", ""))
      });
   }

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("load").where('body[class]', 'chatroom').to(setup, this);

         return this;
      }
   };

}();
