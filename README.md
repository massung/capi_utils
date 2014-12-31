# CAPI Utility Panes for LispWorks

This is a collection of CAPI panes and subclasses that I've built up over time for [LispWorks](http://www.lispworks.com). I use them in many of my projects and I hope you find them useful as well.

Many of these classes are built on the shared knowledge provided in the [Lisp Hug](http://www.lispworks.com/support/lisp-hug.html) mailing list. If are on that list sharing your knowedge with other programmers, please know that my thanks goes out to you.

## `search-text-pane`

By default CAPI doesn't really come with a good search pane. The `text-input-pane` has some initargs allowing it to get the look-and-feel of a search field, but stops there.

Making use of the [placeholder text code](http://www.doremir.com/lisp/lispworks.php) shared by Erik Ronström, I've subclassed `text-input-pane`, and handled callbacks for searching.

***Initargs***

*:search-callback* This is called whenever the user wants to search. The callback function should take the *pane* and *search-text* as arguments.

*:live-search-p* This should be non-nil if the search callback should be called each time the pane's text changes. Otherwise the search callback will only be called when enter is pressed.

***Readers***

*search-text-pane-last-search*

***Accessors***

*search-text-pane-live-search-p*<br/>
*search-text-pane-search-callback*<br/>
*search-text-pane-placeholder-text*
