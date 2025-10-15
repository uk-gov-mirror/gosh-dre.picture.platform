window.onload = function(){

  // Snippet to make sure any external links open in new tab
  $(document.links).filter(function() {
      return this.hostname != window.location.hostname;
  }).attr('target', '_blank');

};
