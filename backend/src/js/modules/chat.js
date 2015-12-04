App.Modules = App.Modules || {};
App.Modules.Chat = function () {

  var o = {
     connection: null,
     id: 1
  };

   var chat = function(data) {
      var message = {
            message_text: $(".js-chat-input").val(),
            uuid: Utils.generateUUID(),
            channel_id: 9,
            type: "incoming_message",
            timestamp: new Date().toISOString()
      };
      o.connection.send(JSON.stringify(message));

      $(".js-chat-input").val("");

      return false;
   };

   var displayMessage = function(response) {
     var p = document.createElement("h2");
     p.appendChild(document.createTextNode(response.e.data));

     $(".js-chat-output").append(p);

     return false;
   };

   var setup = function() {
      var url = App.routes.chatRoom;
      o.connection = new WebSocket(url.replace("http:", "ws:").replace("https:", "wss:").replace("?", "").replace("#", ""));
      o.connection.onmessage = function(e) {
         Events.publish("tl/chat/message/sent", {
            e: e
         });
      };
   }

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("load").where('body[class]', 'chatroom').to(setup, this);
         Events.bind("submit", ".js-chat-form").to(chat, this);

         Events.subscribe("tl/chat/message/sent", displayMessage);

         return this;
      }
   };

}();
