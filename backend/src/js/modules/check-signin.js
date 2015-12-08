App.Modules = App.Modules || {};
App.Modules.CheckSignin = function () {

   var o = {};

   var alertUserToLogin = function(data) {
      if ($('body').data('is-logged-in').toLowerCase() !== "true") {

         if (! _.isUndefined(data.eventElement)) {
            data.eventElement.blur();
         }

         Events.publish("tl/modal/open", {
            modal: 'signin'
         });
      }
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("load").where('body[class]', 'chatroom').to(alertUserToLogin, this);
         Events.bind("focusin", ".js-chat-input").where('body[class]', 'chatroom').to(alertUserToLogin, this);

         return this;
      }
   };

}();
