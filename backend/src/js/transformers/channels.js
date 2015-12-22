App.Transformers = App.Transformers || {};
App.Transformers.channels = function(data) {
   return {
      slug: data.slug,
      color: data.color,
      creatorId: data.creator,
      memberList: data.members,
      timestamp: moment(data.timestamp).format('h:mmA on MM/DD/YYYY'),
      title: data.title,
      description: data.topic,
      activeMembers: data.num_users_present,
      messages: []
   };
};
