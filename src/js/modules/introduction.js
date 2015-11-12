App.Modules = App.Modules || {};
App.Modules.Introduction = function () {
   var o = {
      el: '.js-introduction'
   };

   return {
      init: function() {
         o.module = $(o.el);

         return this;
      },
      events: function() {


         return this;
      }
   };

}();

