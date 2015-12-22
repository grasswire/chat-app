App.Transformers = App.Transformers || {};
App.Transformers.users = function(data) {
   return {
      id: data.user_id,
      profileImageUrl: data.profile_image_url,
      screenName: data.twitter_screen_name,
      presence: data.presence
   };
};
