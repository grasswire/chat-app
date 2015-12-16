App.Modules = App.Modules || {};
App.Modules.Like = function () {

   var o = {};

   var toggleLike = function(data) {
      data.eventElement
          .toggleClass("blurb__like-button")
          .toggleClass("blurb__like-button--liked")
          .find("span")
          .toggleClass("fa-heart-o")
          .toggleClass("fa-heart");
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("click", ".js-like").to(toggleLike, this);

         return this;
      }
   };

}();
