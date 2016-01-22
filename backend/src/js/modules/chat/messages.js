App.Modules = App.Modules || {};
App.Modules.Messages = function () {
  
  var o = { };
  
  var chatReady = function (response) {
    // console.log(response)
    App.socket.onmessage = function(messageEvent) {
      Events.publish("tl/chat/messages/received", {
        message: messageEvent
      });
    };
    
    trackMessagesByChannel(response)
    handleChannelSwitch()
  };
  
  var trackMessagesByChannel = function(rtmStart) {
    // console.log(rtmStart);
    var channels = rtmStart.channels;
    // console.log(channels);
      App.channels.myChannels = _.map(channels, function(channel) { 
        return { 
          channelId: channel.slug, 
          channelMessages: [],
          channelMembers: channel.members
        } 
      });
  };
  
  var handleChannelSwitch = function() {
    $('.js-channel_list_item').click(function(){    
      var messageDisplay = $('.js-chat-output');
      messageDisplay.empty();
      var currentChannel = $(this).attr("data-channel")
      App.data.slug = currentChannel;
      App.channels.current = currentChannel; //  = $(this).attr("data-channel")
      // console.log("channel is set to " + App.channels.current + " now")            
    })
  }
  
  var sendMessage = function(data) {
    var message = {
      message_text: $(".js-chat-input").val(),
      uuid: Utils.generateUUID(),
      channel_id: App.channels.current,
      type: "incoming_message",
      ts: new Date().toISOString()
    };
    
    App.socket.send(JSON.stringify(message));
    $(".js-chat-input").val("");
    
    return false;
  };
  
  var message = function(response) {
    if (_.has(App.Helpers.getQueryParams(location.search), "debug") && s.startsWith(window.location.href, "http://localhost:3000")) {
      // console.log(response.message.data);
    }
    
    try {
      var msg = JSON.parse(response.message.data);
    } catch (e) {
      Events.publish("tl/chat/messages/welcome", {
        text: response.message.data
      });
    }
    
    if (_.has(msg, "type")) {
      // console.log('message is in current channel!!')
      Events.publish("tl/chat/messages/"+msg.type, msg);
      
    }
  };
  
  var welcomeMessage = function(data) {
    display(Handlebars.templates.welcome(data.text));
  };
  
  var standardMessage = function(data) {
    var last = $('.js-blurb').last();
    var latestBlurb = Mapper.item(data, App.Transformers.blurb);
    App.data.memberChannels[latestBlurb.channelId].messages.push(latestBlurb);
    console.log(data.channel);
    console.log(App.channels.current);
    if(data.channel == App.channels.current) {
      if (latestBlurb.channelId == App.data.slug) {
        if (last.data('user-id') == latestBlurb.user.id) {
          display(Handlebars.templates.blurbMultiline(latestBlurb));
          return false;
        }
        display(Handlebars.templates.blurb(latestBlurb));
      }
    } else {
      console.log("received message for other channel: " + data.channel)
    }
  };
  
  var display = function(compiledTemplate) {
    var output = $('.js-chat-output');
    
    output
    .append(App.Helpers.linkifyHashtags(
      App.Helpers.linkifyUsers(compiledTemplate)))
      .linkify({
        defaultProtocol: "//",
        linkClass: "blurb__link",
        linkAttributes: {
          rel: "nofollow"
        }
      })
      .animate({scrollTop: output.prop("scrollHeight")}, 0);
    };
    
    return {
      init: function() { return this; },
      events: function() {
        Events.subscribe("tl/chat/start", chatReady);
        
        Events.subscribe("tl/chat/messages/received", message);
        Events.subscribe("tl/chat/messages/welcome", welcomeMessage);
        Events.subscribe("tl/chat/messages/message", standardMessage);
        
        Events.bind("submit", ".js-chat-form").to(sendMessage, this);
        return this;
      }
    };
    
  }();
