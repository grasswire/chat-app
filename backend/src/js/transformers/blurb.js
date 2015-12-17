App.Transformers = App.Transformers || {};

App.Transformers.blurb = function(item) {
   return {
      messageId: item.uuid,
      timestamp: moment(item.ts).format('h:mm A'),
      text: item.text,
      user: App.data.allMembers[item.user]
   }
};

