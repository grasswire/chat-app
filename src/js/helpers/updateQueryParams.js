App.Helpers = App.Helpers || {};
App.Helpers.updateQueryParams = function(param, value) {
   var re = new RegExp("([?&])" + param + "=.*?(&|#|$)(.*)", "gi"),
       hash,
       url = window.location.href;


    if (re.test(url)) {
        if (! _.isUndefined(value) && ! _.isNull(value) && ! s.isBlank(value))
            return url.replace(re, '$1' + param + "=" + value + '$2$3');
        else if (s.isBlank(value)) {
            var urlparts= url.split('?');
            if (urlparts.length>=2) {

              var prefix= encodeURIComponent(param)+'=';
              var pars= urlparts[1].split(/[&;]/g);

              //reverse iteration as may be destructive
              for (var i= pars.length; i-- > 0;) {
                  //idiom for string.startsWith
                  if (pars[i].lastIndexOf(prefix, 0) !== -1) {
                      pars.splice(i, 1);
                  }
              }

              url= urlparts[0]+'?'+pars.join('&');
              return url;
            } else {
              return url;
            }

        } else {
            hash = url.split('#');
            url = hash[0].replace(re, '$1$3').replace(/(&|\?)$/, '');
            if (! _.isUndefined(hash[1]) && ! _.isNull(hash[1])) {
                url += '#' + hash[1];
            }
            return url;
        }
    }
    else {
        if (! _.isUndefined(value) && ! _.isNull(value)) {
            var separator = url.indexOf('?') !== -1 ? '&' : '?';
            hash = url.split('#');
            url = hash[0] + separator + param + '=' + value;
            if (! _.isUndefined(hash[1]) && ! _.isNull(hash[1])) {
                url += '#' + hash[1];
            }
            return url;
        }
        else
            return url;
    }
};
