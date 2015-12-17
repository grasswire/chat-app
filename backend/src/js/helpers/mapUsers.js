App.Helpers = App.Helpers || {};
App.Helpers.mapUsers = function(userList) {
   var mapped = _.object(
      _.map(
         _.map(userList, function(u) {
            return {
               id: u.user_id,
               profileImageUrl: u.profile_image_url,
               screenName: u.twitter_screen_name
            }
         }),
         function(user) {
            return [user.id, user]
         }
      )
   );
   return mapped;
};
