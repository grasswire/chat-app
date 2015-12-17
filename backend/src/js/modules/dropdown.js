App.Modules = App.Modules || {};
App.Modules.Dropdown = function () {

   var o = {
      dropdown: ".js-dropdown-button"
   };

   var toggleDropdown = function(data) {
      $(".js-dropdown-content[data-dropdown="+data.eventElement.data("dropdown-name")+"]").toggle();
   };

   var hideOnFocus = function(data) {
      $(".js-dropdown-content[data-dropdown="+data.eventElement.find(o.dropdown).data("dropdown-name")+"]").hide();
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("click", o.dropdown).to(toggleDropdown, this);
         Events.bind("mouseleave", ".js-dropdown-parent").to(hideOnFocus, this);
         return this;
      }
   };

}();
