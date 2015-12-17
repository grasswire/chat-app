var Mapper = (function() {

   return {
      collection: function(data, mapperFunc) {
         if (! _.isArray(data)) {
            data = [data];
         }

         return _.map(data, function(item) {
            return mapperFunc(item);
         });
      },

      item: function(data, mapperFunc) {
         if (_.isArray(data)) {
            throw "when calling Mapper.item() the data var needs to be a single object, not an array, if you have an array, you need to use Mapper.collection()";
            return false;
         }

         return mapperFunc(data);
      }
   };
}());
