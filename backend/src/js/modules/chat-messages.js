App.Modules = App.Modules || {};
App.Modules.ChatMessages = function () {

  var o = { };

   var chatReady = function (response) {
      App.socket.onmessage = function(messageEvent) {
         Events.publish("tl/chat/messages/received", {
            message: messageEvent
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

   var message = function(response) {
      try {
         var msg = JSON.parse(response.message.data);
      } catch (e) {
         Events.publish("tl/chat/messages/welcome", {
            text: response.message.data
         });
      }

      if (_.has(msg, "type")) {
         Events.publish("tl/chat/messages/"+msg.type, msg);
      }

   };

   var welcome = function(data) {
      $('.js-chat-output').append(Handlebars.templates.welcome(data.text));
   };

   var blurb = function(data) {
      var transformer = {
         text: data.text,
         user: App.data.users[data.user]
      };
      $('.js-chat-output').append(Handlebars.templates.blurb(transformer));
   };


   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe("tl/chat/socket/ready", chatReady);

         Events.subscribe("tl/chat/messages/received", message);
         Events.subscribe("tl/chat/messages/welcome", welcome);
         Events.subscribe("tl/chat/messages/message", blurb);

         Events.bind("submit", ".js-chat-form").to(sendMessage, this);
         return this;
      }
   };

}();
