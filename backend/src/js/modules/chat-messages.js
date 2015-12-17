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
            channel_id: $('body').data('channel'),
            type: "incoming_message",
            ts: new Date().toISOString()
      };

      App.socket.send(JSON.stringify(message));
      $(".js-chat-input").val("");

      return false;
   };

   var message = function(response) {
      try {
         var msg = JSON.parse(response.message.data);
         console.log(msg);
      } catch (e) {
         Events.publish("tl/chat/messages/welcome", {
            text: response.message.data
         });
      }

      if (_.has(msg, "type")) {
         Events.publish("tl/chat/messages/"+msg.type, msg);
      }

   };

   var welcomeMessage = function(data) {
      display(Handlebars.templates.welcome(data.text));
   };

   var standardMessage = function(data) {
      var transformer = {
         messageId: data.uuid,
         timestamp: moment(data.ts).format('h:mm:ss A'),
         text: data.text,
         user: App.data.users[data.user]
      };

      display(Handlebars.templates.blurb(transformer));
   };

   var display = function(compiledTemplate) {
      var output = $('.js-chat-output');

      output
         .append(App.Helpers.linkifyHashtags(
                 App.Helpers.linkifyUsers(compiledTemplate)))
         .linkify({
            defaultProtocol: "//",
            linkClass: "blurb__link",
            linkAttributes: {
               rel: "nofollow"
            }
         })
         .animate({scrollTop: output.prop("scrollHeight")}, 0);
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe("tl/chat/socket/ready", chatReady);

         Events.subscribe("tl/chat/messages/received", message);
         Events.subscribe("tl/chat/messages/welcome", welcomeMessage);
         Events.subscribe("tl/chat/messages/message", standardMessage);

         Events.bind("submit", ".js-chat-form").to(sendMessage, this);
         return this;
      }
   };

}();
