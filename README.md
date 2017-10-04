Zoom
====

This minor mode takes care of managing the window sizes by enforcing a fixed and
automatic balanced layout where the currently selected window is resized
according to `zoom-size` which can be either an absolute value in rows/columns
or a ratio between the selected window and frame size.

TODO screencast

Installation
------------

### Manual

Add the following to your init file:

```el
(require 'zoom-mode "/path/to/zoom-mode.el")
```

### TODO melpa or such

Usage
-----

Enable this minor mode with `M-x zoom-mode` otherwise use `M-x zoom` to manually
rearrange windows just once.

To load `zoom-mode` automatically add one of the following to your init file:

```el
(custom-set-variables
 '(zoom-mode t))
```

or:

```el
(zoom-mode t)
```

Example configurations
----------------------

Resize the selected window according to the [golden ratio].

```el
(custom-set-variables
 '(zoom-size '(0.61803399 0.61803399)))
```

[golden ratio]: https://en.wikipedia.org/wiki/Golden_ratio

Follows all the possible ways to blacklist windows, for example:
- `dired` and `markdown` major modes;
- `zoom-mode.el` `init.el` buffer names;
- calculator-related windows;
- any buffer containing less than 20 lines.

```el
(custom-set-variables
 '(zoom-ignored-major-modes '(dired-mode markdown-mode))
 '(zoom-ignored-buffer-names '("zoom-mode.el" "init.el"))
 '(zoom-ignored-buffer-name-regexps '("^*calc"))
 '(zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20)))))
```

(Please note that ignored windows are not resized when selected but all the
windows are nevertheless arranged with `balance-windows`.)

### What about [`golden-ratio.el`]?

I have been a more or less happy [`golden-ratio.el`] user for some time when I
stared noticing some bugs and sadly I discovered that it is apparently a dead
project now, so I decided to write a new and improved minor mode from scratch
that implements the same basic idea of automatic window layout as my first
attempt at Emacs mode development.

[`golden-ratio.el`]: https://github.com/roman/golden-ratio.el
