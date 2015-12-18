App.Modules = App.Modules || {};
App.Modules.Modal = function () {

   var openModal = function(data) {
      var modalName = _.has(data, "eventElement") ? data.eventElement.attr("data-modal-trigger") : data.modal;
      $('.js-modal[data-modal-name='+modalName+']').attr('data-modal-open', true).fadeIn(50);
      $('body').css('overflow', 'hidden');
   };

   var closeModal = function(data) {
      var openModal = $('.js-modal[data-modal-open=true]');
      openModal.fadeOut(50).attr('data-modal-open', false);
      $('body').css('overflow', '');
      var params = App.Helpers.getQueryParams(location.search);

      if (_.has(params, "modal")) {
         var url = App.Helpers.updateQueryParams('modal', '');
         // Removes the query param without the refreshing the page
         window.history.pushState("", "", url);
      }

      Events.publish("tl/modal/closed", {
         modal: openModal
      });
   };

   var checkQueryString = function() {
      if (! s.isBlank(location.search)) {
         var params = App.Helpers.getQueryParams(location.search);
         if (_.has(params, "modal")) {
            Events.publish("tl/modal/open", {
               modal: params.modal
            });
         }
      }
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.bind("load").to(checkQueryString, window);
         Events.bind("click", ".js-trigger-modal").to(openModal);
         Events.bind("click", ".js-trigger-modal-close").to(closeModal);
         Events.bind("key", [27]).to(closeModal, window);

         Events.subscribe('tl/modal/closing', closeModal);
         Events.subscribe('tl/modal/open', openModal);

         return this;
      }
   };

}();
