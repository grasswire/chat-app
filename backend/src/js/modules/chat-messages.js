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
            message: e
         });
      };
   };

   var displayMessage = function(response) {
      $('.js-chat-output').append(Handlebars.templates.blurb(response.message));
      return false;
   };

   var messageReceived = function(response) {
      try {
         var msg = JSON.parse(response.message.data);
         var message = {
            text: msg.text
         };

      } catch (e) {
         var message = {
            text: response.message.data
         };
      };

      Events.publish("tl/chat/message/received", {
         message: message
      });
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe("tl/chat/setup", chatReady);
         Events.subscribe("tl/chat/message/sent", messageReceived);
         Events.subscribe("tl/chat/message/received", displayMessage);

         Events.bind("submit", ".js-chat-form").to(sendMessage, this);
         return this;
      }
   };

}();
