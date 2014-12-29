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

## `output-panel`

An output-panel is a subclass of both `output-pane` and `collection`. It also mimics the behavior of `choice`. You can think of an `output-panel` as a very efficient version of a `column-layout` filled with `output-pane` elements and can select them. It takes care of all drawing, scrolling, input  and selection (`:no-selection`, `:single-selection`, and `:multiple-selection`). 

*Treat it like you would a `list-panel`, but where you can draw anything you want for each item.*

***Initargs***

*:interaction* This is the selection interaction style. It follows other CAPI conventions and can be `nil`, `:no-selection`, `:single-selection`, or `:multiple-selection` (`:extended-selection` is treated the same as `:multiple-selection`). The default value is `nil`.

*:item-height* This is the height (in pixels) that each item in the collection will require to display itself.

*:item-display-callback* This is called during the display of the panel. Each item that is within the panel's visible border will have this called with the *panel*, *item*, *width*, *height*, and a generalized boolean indicating whether or not the item is currently *selected*.

*:item-action-callback* This is called with the *panel* and the *item* in the event that the item is double clicked.

*:item-selected-callback* This is called whenever the *item* has been selected by the user or programmatically. It takes the *panel* and the *item* as parameters.

*:item-retract-callback* This is called whenever the *item* has been removed from the current selection by the user or programmatically. It takes the *panel* and the *item* as parameters. It is not called if the collection changes.

*:item-menu* This is a menu that will be displayed when an item (or multiple items) is selected and right-clicked.

*:selected-background* This is the color to use when drawing the background of a selected item. It defaults to `:color_highlight`.

*:selected-foreground* This is the color to use when drawing the foreground of a selected item. It defaults to `:color_highlighttext`.

*:selection-callback* This is called whenever the selection changes. This is called in addition to the *:item-selected-callback*

***Accessors***

*output-panel-item-height*<br/>
*output-panel-item-menu*<br/>
*output-panel-item-display-callback*<br/>
*output-panel-item-action-callback*<br/>
*output-panel-item-select-callback*<br/>
*output-panel-item-retract-callback*<br/>
*output-panel-selected-background*<br/>
*output-panel-selected-foreground*<br/>
*output-panel-interaction*<br/>
*output-panel-selection-callback*<br/>
*output-panel-selected-items*<br/>
*output-panel-selection*

***Methods***

`(output-panel-selected-item-p panel item)` returns T if *item* is currently selected. Remember that *item* is tested against the currently selected items with *collection-test-function*.

`(output-panel-sort panel predicate &key key)` sorts all the items in the collection with *stable-sort*. The current selection is maintained.

`(output-panel-select-all panel)` selects all the items in the collection if the *:interaction* is `:multiple-selection`. Otherwise no change to the selection is made.

***Notes***

Unlike the `choice` class, when getting the current selection a list will *always* be returned - even if `:single-selection` is the interaction style.

The `output-panel-selection` accessor returns item indices, while `output-panel-selected-items` returns the actual items. Both are `setf`-able. But, when setting the selected items, `search-for-item` will be used to find the index, which uses the `collection-test-function`. If you have multiple items in then collection that test equal, the first one will be selected.

When your item is being drawn, its draw area is transformed and masked by the panel. This means that (0,0) is the upper-left coordinate of the item's visible area (and not the upper-left of the panel). It also means that if you draw outside the visible area of the item it will be clipped.

***Example***

	(defun draw-item (panel item w h selected-p)
	  (let ((y (gp:get-font-ascent panel)))
		(gp:draw-string panel item 0 y)))
		
	(defun selection-changed (panel)
	  (let ((items (output-panel-selected-items panel)))
	    (display-message "Selected items:~{~%~s~}" items)))
		
	(contain (make-instance
	          'output-panel
	          :interaction :multiple-selection
	          :selection-callback 'selection-changed
	          :item-height 20
	          :item-display-callback 'draw-item
	          :items '("Me" "Myself" "I")))

