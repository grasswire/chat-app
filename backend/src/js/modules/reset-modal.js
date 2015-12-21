App.Modules = App.Modules || {};
App.Modules.ResetModal = function () {

   var resetModal = function(data) {
      var form = data.modal.find('form');
      form.find('input:text, input:password, input:file, select, textarea').val('');
      form.find('input:radio, input:checkbox')
           .removeAttr('checked').removeAttr('selected');
      form.find('input.error, select.error, textarea.error').removeClass("error")
      form.find('label.error').detach();
   };

   return {
      init: function() { return this; },
      events: function() {
         Events.subscribe('tl/modal/closed', resetModal);

         return this;
      }
   };

}();
