css-sort.el
===========

Sorts css and scss attributes "concentrically". Plays nice with scss-lint.

## Installing

Stick the repo somewhere. Then do something like: 

```
(add-to-list 'load-path "~/.emacs.d/css-sort/")
(require 'css-sort)
(with-eval-after-load "scss-mode"
  (define-key scss-mode-map (kbd "C-c C-s") 'css-sort-attributes))
```

Plop point into a morass of css attributes, and watch <kbd>C-c C-s</kbd> order them AS THOUGH BY MAGIC.

MAGIC.
