{
  var pattern1 = new RegExp ('^http://(tkb.mpl.com|(www.|)unwind-protect.org)');
  var pattern2 = new RegExp ('^http:');
  var links = document.links;
  var i;
  var limit;
  var n = links.length;
  var h = new Array(n);
  var t = new Array(n);

  if (window.tkb_n === undefined)
      window.tkb_n = 0;
  else
      window.tkb_n += 10;
  limit = window.tkb_n + 10;
  if (limit > n) limit = n;
  for (i = window.tkb_n; i < limit; i ++) {
    h[i] = links[i].href;
    t[i] = links[i].text;
  }
  for (i = window.tkb_n; i < limit; i ++) {
      if (! (h[i].match (pattern1))) {
          if (h[i].match (pattern2)) {
	      var w = open (h[i]);
	  }
      }
  }
  document.close();
}
