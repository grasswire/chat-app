(function() {
   var App = {
      routes: {

      },
      init: function () {
         _.each(App.Modules, function(key, value) {
            App.Modules[value].init().events();
         });
      }
   };

   window.App = App;
})();



