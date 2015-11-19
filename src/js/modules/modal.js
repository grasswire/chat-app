App.Modules = App.Modules || {};
App.Modules.Modal = function () {

   var openModal = function(data) {
      var modalName = data.eventElement.attr("data-modal-trigger");
      $('.js-modal[data-modal-name='+modalName+']').attr('data-modal-open', true).fadeIn(50);
   };

   var closeModal = function(data) {
      var openModal = $('.js-modal[data-modal-open=true]');
      openModal.fadeOut(50).attr('data-modal-open', false);
      Events.publish("tl/modal/closed", {
         modal: openModal
      });
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("click", ".js-trigger-modal").to(openModal);
         Events.bind("click", ".js-trigger-modal-close").to(closeModal);

         Events.subscribe('tl/modal/closing', closeModal);

         return this;
      }
   };

}();
