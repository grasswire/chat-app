App.Modules = App.Modules || {};
App.Modules.Sidebar = function () {

   var o = {
      threshold: 768
   };

   var toggleSidebar = function(data) {
      var container = $(".js-sidebar");

      if (container.attr('data-container-closed') == "true") {
         container.attr('data-container-closed', "false")
                  .css({
                     minWidth: "240px",
                     overflowX: "hidden",
                     overflowY: "scroll",
                     width: "30%",
                  })
                  .find(".js-sidebar-contents").show();
         return false;
      }

      container.attr('data-container-closed', "true")
               .css({
                  minWidth: "auto",
                  overflow: "hidden",
                  width: "50px"
               })
               .find(".js-sidebar-contents").hide();
      return false;
   };

   var checkPageWidth = function() {
      var container = $(".js-sidebar"),
          pageWidth = parseInt($("body").outerWidth(true));

      if (container.attr('data-container-closed') != "true" && pageWidth <= o.threshold) {
         $(".js-sidebar-toggler").trigger("click");
      }
   };
   
   var $channelsItems = $('.js-channel_list_item').click(function(){
     $channelsItems.removeClass("room__channel-selected");
     $(this).addClass( "room__channel-selected" );
   })


   return {
      init: function() { return this; },
      events: function() {
         // Events.bind("load").where("body[class]", "chatroom").to(checkPageWidth, this);
         // Events.bind("resize").where("body[class]", "chatroom").to(checkPageWidth, this);
         // Events.bind("click", ".js-sidebar-toggler").to(toggleSidebar, this);
         return this;
      }
   };

}();
