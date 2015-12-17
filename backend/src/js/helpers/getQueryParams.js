App.Helpers = App.Helpers || {};
App.Helpers.getQueryParams = function(queryString) {
   queryString = queryString.replace("?", "");
   var params = queryString.split("&"),
       paramObject = {};

   if (!s.isBlank(queryString)) {
      _.each(params, function(item){
         var pair = item.split("=");
         if (!_.isUndefined(pair[1])) {
            paramObject[pair[0]] = pair[1];
         } else {
            paramObject[pair[0]] = pair[0];
         }
      });
   }

   return paramObject;
}
