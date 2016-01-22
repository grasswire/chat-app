(function() {
   var App = {
      channels: { },
      routes: { },
      data: { },
      socket: null,
      init: function () {
         _.each(App.Modules, function(key, value) {
            App.Modules[value].init().events();
         });

      }
   };

   window.App = App;
})();



