App.Modules = App.Modules || {};
App.Modules.CheckSignin = function () {

   var o = {};

   var checkLogin = function() {
      var isLoggedIn = ($('body').data('is-logged-in').toLowerCase() === "true") ? true : false;

      if (! isLoggedIn) {
         Events.publish("tl/modal/open", {
            modal: 'signin'
         });
      }
   };

   var noWriting = function(data) {
      checkLogin();
      data.eventElement.blur();
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("load").where('body[class]', 'chatroom').to(checkLogin, this);
         Events.bind("focusin", ".js-chat-input").to(noWriting, this);
         return this;
      }
   };

}();
