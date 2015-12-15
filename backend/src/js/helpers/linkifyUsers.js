App.Helpers = App.Helpers || {};
App.Helpers.linkifyUsers = function(blurb) {
   blurb = blurb.replace(/(^|\s)#(\w+)/g, '$1<a href="#$2" target="_blank" rel="nofollow" class="blurb__link--hashtag">#$2</a>');
   return blurb.replace(/(^|\s)@(\w+)/g, '$1<a href="#$2" target="_blank" rel="nofollow" class="blurb__link--username">@$2</a>');
}
