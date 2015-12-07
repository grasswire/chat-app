App.Modules = App.Modules || {};
App.Modules.Dropdown = function () {

   var o = {
      dropdown: ".js-dropdown-button"
   };

   var toggleDropdown = function(data) {
      $(".js-dropdown-content[data-dropdown="+data.eventElement.data("dropdown")+"]").toggle();
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("click", o.dropdown).to(toggleDropdown, this);
         return this;
      }
   };

}();
