App.Modules = App.Modules || {};
App.Modules.ColorPicker = function () {

   var o = {};

   var setNewRoomColor = function(data) {
      $(".js-create-color.js-color-selected").removeClass("js-color-selected").parent().removeClass("selected");
      data.eventElement.addClass("js-color-selected").parent().addClass("selected");
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("click", ".js-create-color").to(setNewRoomColor, this);

         return this;
      }
   };

}();
