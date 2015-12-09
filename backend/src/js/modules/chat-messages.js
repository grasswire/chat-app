App.Modules = App.Modules || {};
App.Modules.ChatMessages = function () {

  var o = { };

   var chatReady = function (response) {
      App.socket.onmessage = function(e) {
         Events.publish("tl/chat/messages/received", {
            message: e
         });
      };
   };

   var sendMessage = function(data) {
      var message = {
            message_text: $(".js-chat-input").val(),
            uuid: Utils.generateUUID(),
            channel_id: 9,
            type: "incoming_message",
            ts: new Date().toISOString()
      };

      App.socket.send(JSON.stringify(message));
      $(".js-chat-input").val("");

      return false;
   };

   var displayMessage = function(response) {
      $('.js-chat-output').append(Handlebars.templates.blurb(response.message));
      return false;
   };

   var messageReceived = function(response) {
      try {
         var msg = JSON.parse(response.message.data);
         if (msg.type === "message") {
            var message = {
               type: true,
               text: msg.text,
               user: App.data.users[msg.user]
            };
            console.log(message);
         }
      } catch (e) {
         var message = {
            text: response.message.data
         };
      };

      Events.publish("tl/chat/messages/parsed", {
         message: message
      });
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe("tl/chat/socket/ready", chatReady);
         Events.subscribe("tl/chat/messages/received", messageReceived);
         Events.subscribe("tl/chat/messages/parsed", displayMessage);

         Events.bind("submit", ".js-chat-form").to(sendMessage, this);
         return this;
      }
   };

}();
