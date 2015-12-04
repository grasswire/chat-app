App.Modules = App.Modules || {};
App.Modules.ChatMessages = function () {

  var o = {
     connection: null
  };

   var sendMessage = function(data) {
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

   var chatReady = function (response) {
      o.connection = response.connection;

      o.connection.onmessage = function(e) {
         Events.publish("tl/chat/message/sent", {
            e: e
         });
      };
   };

   var displayMessage = function(response) {
     var p = document.createElement("h2");
     p.appendChild(document.createTextNode(response.e.data));

     $(".js-chat-output").append(p);

     return false;
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe("tl/chat/setup", chatReady);
         Events.subscribe("tl/chat/message/sent", displayMessage);

         Events.bind("submit", ".js-chat-form").to(sendMessage, this);
         return this;
      }
   };

}();
