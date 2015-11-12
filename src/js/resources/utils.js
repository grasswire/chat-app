var Utils = (function(){

   return {
      generateUUID: function() {
         var d = Date.now();
         var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
            var r = (d + Math.random()*16)%16 | 0;
            d = Math.floor(d/16);
            return (c=='x' ? r : (r&0x3|0x8)).toString(16);
         });

         return uuid;
      },
      generateEventName: function(asEventName) {
         if (_.isUndefined(asEventName)) {
            return "app/event/" + Utils.generateUUID();
         }

         return asEventName;
      }
   };
}());
