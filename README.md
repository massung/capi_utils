# CAPI Utility Panes for LispWorks

This is a collection of CAPI panes and subclasses that I've built up over time for [LispWorks](http://www.lispworks.com). I use them in many of my projects and I hope you find them useful as well.

Many of these classes are built on the shared knowledge provided in the [Lisp Hug](http://www.lispworks.com/support/lisp-hug.html) mailing list. If are on that list sharing your knowedge with other programmers, please know that my thanks goes out to you.

## `search-text-pane`

By default CAPI doesn't really come with a good search pane. The `text-input-pane` has some initargs allowing it to get the look-and-feel of a search field, but stops there.

Making use of the [placeholder text code](http://www.doremir.com/lisp/lispworks.php) shared by Erik Ronström, I've subclassed `text-input-pane`, and handled callbacks for searching.

***Initargs***

*:soft-search-callback* This is called whenever the user changes the text in the pane. The callback function should take the *pane* and *search-text* as arguments.

*:hard-search-callback* This is called whenever the user has pressed enter, return, shift-enter, or shift-return inside the pane. Like the *soft-search-callback*, it takes the pane and the text as arguments.

***Readers***

*search-text-pane-last-search* Always returns the text of the last **hard** search performed (the last soft search is always equal to *text-input-pane-text*).

***Accessors***

*search-text-pane-soft-search-callback*<br/>
*search-text-pane-hard-search-callback*<br/>
*search-text-pane-placeholder-text*
