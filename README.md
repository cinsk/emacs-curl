emacs-curl
==========

This package provides CURL interface to Emacs.

It provides various functions to interact with external curl(1) program.  Currently, it support HTTP only, and provides several functions to handle HTTP GET, HEAD, POST, PUT, DELETE methods.

Asynchronous HTTP requests are not fully implemented yet.  I'm intending to implement it soon.

The original idea and some of codes are stealed from the g-client.  See also http://code.google.com/p/emacspeak/ .


Why not url package?
--------------------

Emacs ships the URL handling functions such as `url-retrieve` or `url-retrieve-synchronously` in `url.el`, which makes emacs-curl somewhat redundant.

If you do not have any specific reason, please use url package instead emacs-curl.

Compared to url package, using emacs-curl has pros and cons:

* cons: external dependency on CURL(1)
* pros: precise controlling the connection.
* pros: for the long-term operation, emacs-curl is faster than the url package.

