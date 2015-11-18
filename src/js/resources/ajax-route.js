var AjaxRoute = (function() {

   var ro = {
      url: "",
      name: "",
      type: "",
      data: {},
      callType: "",
      userData: {},
      useQueryParams: false,
      extraAjaxParams: null,
   };

   var mergeParams = function(response, userData) {
      response = _.isString(response) ? JSON.parse(response) : _.isArray(response) ? {responseArray: response} : response;

      return _.extend(response, userData);
   };

   var sendAjaxRequest = function() {
      $.ajax(paramSetup(ro.extraAjaxParams))
         .done(function(response) {
            var params = mergeParams(response, ro.userData);

            Events.publish(ro.name+"/success", params);
         })
         .fail(function(response) {
            var params = mergeParams(response, ro.userData);

            Events.publish(ro.name+"/fail", params);
         })
         .always(function(response) {
            var params = mergeParams(response, ro.userData);

            Events.publish(ro.name+"/complete", params);
         });
   };

   var paramSetup = function(mergeParams) {
      var params  = {};

      if (ro.type == "get" || ro.useQueryParams) {
         params = {
            type: ro.type,
            url: ro.url
         };
      } else {
         params = {
            type: ro.type,
            url: ro.url,
            dataType: "json",
            contentType: "application/json",
            processData: false,
            data: ro.data
         };
      }

      if (! _.isNull(mergeParams)) {
         return _.extend(params, mergeParams);
      }

      return params;
   };

   return {
      as: function(type, useQueryParams, extraAjaxParams) {
         ro.type = type;
         ro.useQueryParams = _.isUndefined(useQueryParams) || ! _.isBoolean(useQueryParams) ? false : useQueryParams;
         ro.extraAjaxParams = extraAjaxParams || null;

         return this;
      },

      to: function(url, data) {
         ro.name = Utils.generateEventName();
         ro.url = url;
         ro.data = JSON.stringify(data);

         return this;
      },

      on: function(responseTypes, context, userData) {
         ro.userData = userData || {};

         _.each(responseTypes, function(value, key) {
            Events.subscribe(ro.name+"/"+key, value, context);
         });

         sendAjaxRequest();
         return false;
      }
   };
}());
