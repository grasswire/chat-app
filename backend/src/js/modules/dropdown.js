App.Modules = App.Modules || {};
App.Modules.Dropdown = function () {

   var o = {
      dropdown: ".js-dropdown"
   };

   var toggleDropdown = function(data) {
      console.log(data);
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("click", o.dropdown).to("toggleDropdown", this);
         return this;
      }
   };

}();
