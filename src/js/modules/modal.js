App.Modules = App.Modules || {};
App.Modules.Modal = function () {

   var openModal = function(data) {
      var modalName = data.eventElement.attr("data-modal-trigger");
      $('.js-modal[data-modal-name='+modalName+']').fadeIn(50);
   };

   var closeModal = function(data) {
      data.eventElement.closest(".js-modal").fadeOut(50);
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("click", ".js-trigger-modal").to(openModal);
         Events.bind("click", ".js-trigger-modal-close").to(closeModal);

         return this;
      }
   };

}();

