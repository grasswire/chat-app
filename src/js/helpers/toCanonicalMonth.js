App.Helpers = App.Helpers || {};
App.Helpers.toCanonicalMonth = function(month) {
   return month < 10 ? "0" + month : month;
};


