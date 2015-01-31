modeline-pack
=============

This is a pack to manipulate the emacs-modeline (cf. http://jr0cket.co.uk/2013/01/tweeking-emacs-modeline-for-clojure.html)

# Install

This is compatible with [emacs-live-packs](https://github.com/ardumont/emacs-live-packs) and [prelude-packs](https://github.com/ardumont/prelude-packs).

## [emacs-live-packs](https://github.com/ardumont/emacs-live-packs)

Add this snippet in your `.emacs-live.el`:
```elisp
(emacs-live-packs/add-live-packs "~/.emacs-live-packs/" '("modeline-pack"))
```

## [prelude-packs](https://github.com/ardumont/prelude-packs)

Add this snippet in your `prelude-packs.el`:
```elisp
(prelude-packs/load-packs "~/.prelude-packs/" '("modeline-pack"))
```
