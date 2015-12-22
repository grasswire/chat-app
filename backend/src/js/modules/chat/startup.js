App.Modules = App.Modules || {};
App.Modules.Startup = function () {

   var o = { };

   var rtmStart = function() {
      AjaxRoute.as("get")
         .to(App.routes.rtmStartUrl, {})
         .on({
            success: socketStart
         });
   };

   var socketStart = function(response) {
      var url = App.routes.chatRoom;
      App.socket = new WebSocket(url.replace("http:", "ws:").replace("https:", "wss:").replace("?", "").replace("#", ""));

      Events.publish("tl/chat/start", response);
   }

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("load").where('body[class]', 'chatroom').to(rtmStart);
         return this;
      }
   };

}();
