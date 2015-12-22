App.Modules = App.Modules || {};
App.Modules.Startup = function () {

   var o = { };

   var rtmStart = function() {
      AjaxRoute.as("get")
         .to(App.routes.rtmStartUrl, {})
         .on({
            success: payloadSetup
         });
   };

   var payloadSetup = function(response) {
      if (! _.isUndefined(response.self)) {
         response.users.push(response.self);
         App.data.thisMember = Mapper.item(response.self, App.Transformers.users);
      }

      App.data.allMembers = _.object(
         Mapper.collection(
            Mapper.collection(response.users, App.Transformers.users),
               function(item) { return [item.id, item]}
            )
         );

      App.data.memberChannels = _.object(
         Mapper.collection(
            Mapper.collection(response.channels, App.Transformers.channels),
               function(item) { return [item.slug, item]}
            )
         );

      Events.publish("tl/chat/payload/ready", response);
   };

   var socketStart = function(response) {
      var url = App.routes.chatRoom;
      App.socket = new WebSocket(url.replace("http:", "ws:").replace("https:", "wss:").replace("?", "").replace("#", ""));

      Events.publish("tl/chat/start", response);
   };

   var roomStart = function(response) {
      App.data.slug = $('body').data('channel');
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("load").where('body[class]', 'chatroom').to(rtmStart);
         Events.subscribe("tl/chat/payload/ready", roomStart);
         Events.subscribe("tl/chat/payload/ready", socketStart);

         return this;
      }
   };

}();
