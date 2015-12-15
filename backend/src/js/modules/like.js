App.Modules = App.Modules || {};
App.Modules.Like = function () {

   var o = {};

   var toggleLike = function(data) {

   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("click", ".js-like").to(toggleLike, this);

         return this;
      }
   };

}();
