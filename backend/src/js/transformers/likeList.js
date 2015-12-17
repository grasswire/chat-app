App.Transformers = App.Transformers || {};
App.Transformers.likeList = function(data) {
   return {
      messageId: data.uuid,
      timestamp: moment(data.ts).format('h:mm A'),
      text: data.text,
      user: App.data.allMembers[data.user],
      likes: data.likes
   };
};
