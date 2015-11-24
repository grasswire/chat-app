App.Modules = App.Modules || {};
App.Modules.Chat = function () {

  var guid = function() {
    function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
      .toString(16)
      .substring(1);
    }
  return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
    s4() + '-' + s4() + s4() + s4();
  };

  var o = {
     connection: null,
     id: 1
  };

   var chat = function(data) {
      var message = {
            message_text: $(".js-chat-input").val(),
            uuid: guid(),
            channel_id: 9,
            type: "incoming_message",
            timestamp: new Date().toISOString()
      };

      o.connection.send(JSON.stringify(message));


      $(".js-chat-input").val("");
      return false;
   };

   var displayMessage = function(data) {
     var p = document.createElement("p");
     p.appendChild(document.createTextNode(data.e.data));
     $(".js-chat-output").append(p);
     return false;
   };

   var setup = function() {
      var url = App.routes.chatRoom
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
         Events.bind("load").where('div', 'page-chat-room').to(setup, this);
         Events.bind("submit", ".js-chat-form").to(chat, this);

         Events.subscribe("tl/chat/message/sent", displayMessage);

         return this;
      }
   };

}();
