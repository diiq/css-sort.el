;;; css-sort.el --- Sort css concentrically

;; Author: Sam Bleckley <s@diiq.org>
;; Version: 0.1
;; Keywords: tools, sort, css, scss, sass, concentric

;;; Commentary:

;; This package provides functions for sorting css attribute
;; declarations. Currently the only truly interesting function is
;; 'sort-css-attributes-in-region .


(setq css-sort-attributes-order
      '("display"
        "position"
        "top"
        "right"
        "bottom"
        "left"
        "float"
        "clear"
        "visibility"
        "opacity"
        "z-index"
        "margin"
        "margin-top"
        "margin-right"
        "margin-bottom"
        "margin-left"
        "outline"
        "border"
        "border-top"
        "border-right"
        "border-bottom"
        "border-left"
        "border-width"
        "border-top-width"
        "border-right-width"
        "border-bottom-width"
        "border-left-width"
        "border-style"
        "border-top-style"
        "border-right-style"
        "border-bottom-style"
        "border-left-style"
        "border-color"
        "border-top-color"
        "border-right-color"
        "border-bottom-color"
        "border-left-color"
        "background"
        "background-color"
        "background-image"
        "background-repeat"
        "background-position"
        "cursor"
        "padding"
        "padding-top"
        "padding-right"
        "padding-bottom"
        "padding-left"
        "width"
        "min-width"
        "max-width"
        "height"
        "min-height"
        "max-height"
        "overflow"
        "list-style"
        "caption-side"
        "table-layout"
        "border-collapse"
        "border-spacing"
        "empty-cells"
        "vertical-align"
        "text-align"
        "text-indent"
        "text-transform"
        "text-decoration"
        "line-height"
        "word-spacing"
        "letter-spacing"
        "white-space"
        "color"
        "font"
        "font-family"
        "font-size"
        "font-weight"
        "content"
        "quotes"))

(defun css-sort-index (object list)
  "return the index of object in list"
  (let ((counter 0)
        (found nil))
    (catch 'finished
      (dolist (listelement list counter)
        (if (equal object listelement)
            (progn
              (setq found t)
              (throw 'finished counter))
          ;; else increment counter
          (incf counter)))
    ;; if we found it return counter otherwise return nil
    (if found counter nil))))

; (css-sort-index 'a '(b c a d)) => 2
; (css-sort-index "background" css-sort-attributes-order)

(defun css-sort-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

; (css-sort-chomp "    alan  ricky    ") => "alan  ricky"

(defun css-sort-attribute-from-line (line)
  (css-sort-chomp (nth 0 (split-string line ":"))))

; (css-attribute-from-line "    color: symople;") => "color"

(defun css-sort-attribute-index (line)
  (or (css-sort-index
       (css-sort-attribute-from-line line) css-sort-attributes-order)
      -1))

; (css-sort-attribute-index "    color: aslfa") => 70

(defun css-attribute-sort-compare (a b)
  (< (css-sort-attribute-index a)
     (css-sort-attribute-index b)))

; (css-attribute-sort-compare "    position: symfo" "   background: qwants") => 't

(defun css-sort-lines-in-region (start end)
  (split-string (buffer-substring start end) "[\n]"))


(defun css-sort-beginning-of-attribute-block (start)
  (goto-char start)
  (search-backward "{")
  (forward-line 1)
  (beginning-of-line)
  (point))

; { (beginning-of-attribute-block (point)) => 4044 or something

(defun css-sort-end-of-attribute-block (start)
  (goto-char start)
  (re-search-forward "[{}]")
  (forward-line -1)
  (end-of-line)
  (point))

;;;###autoload
(defun css-sort-attributes (start end)
  (interactive "r")
  (save-excursion
    (let* ((current (point))
           (start (css-sort-beginning-of-attribute-block current))
           (end (css-sort-end-of-attribute-block current))
           (lines (css-sort-lines-in-region start end))
           (sorted-lines (sort lines #'css-attribute-sort-compare)))
      (delete-region start end)
      (save-excursion
        (goto-char start)
        (insert (mapconcat 'identity sorted-lines "\n"))))))

(provide 'css-sort)
