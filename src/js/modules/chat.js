App.Modules = App.Modules || {};
App.Modules.Chat = function () {

   var o = {
      connection: null,
      id: 1
   };

   var send = function(data) {
      var message = {
            message: $(".js-chat-input").val(),
            id: o.id++
      };

      o.connection.send(JSON.stringify(message));
      o.connection.onmessage = function(e) {
         Events.publish("tl/chat/message/sent", {
            e: e
         });
      };

      $(".js-chat-input").val("");
      return false;
   };

   var displayMessage = function(data) {
     var p = document.createElement("p");
     p.appendChild(document.createTextNode(data.e.data));
     $(".js-chat-output").append(p);
     return false;
  };

   return {
      init: function() {
         var url = document.location.href;
         o.connection = new WebSocket(url.replace("http:", "ws:").replace("https:", "wss:").replace("?", "").replace("#", ""))

         return this;
      },
      events: function() {
         Events.bind("submit", ".js-chat-form").to(send, this);
         Events.subscribe("tl/chat/message/sent", displayMessage);
         return this;
      }
   };

}();
