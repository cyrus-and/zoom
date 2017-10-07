;;; zoom.el --- Fixed and automatic balanced window layout

;; Copyright (c) 2017 Andrea Cardaci <cyrus.and@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: Andrea Cardaci <cyrus.and@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/cyrus-and/zoom
;; Package-Requires: ((emacs "24.4"))
;; Keywords: frames

;;; Commentary:

;; This minor mode takes care of managing the window sizes by enforcing a fixed
;; and automatic balanced layout where the currently selected window is resized
;; according to `zoom-size' which can be an absolute value in lines/columns, a
;; ratio between the selected window and frame size or even a custom callback.

;;; Code:

(defgroup zoom nil
  "Enforce a fixed and automatic balanced window layout."
  :group 'windows)

(defcustom zoom-size '(80 . 24)
  "Size hint for the selected window.

It can be either a cons or a function.

Each component of the cons can be either an absolute value in
lines/columns or a ratio between the selected window and the
frame size.  In the former case the window is resized according
to its body size, i.e., the total window size can be much larger.
In any case, windows are never shrinked if they are already
larger than the resulting size.

The function takes no arguments and returns a cons as specified
above."
  :type '(choice (function :tag "Custom")
                 (cons :tag "Fixed"
                       (choice (integer :tag "Columns")
                               (float :tag "Width ratio"))
                       (choice (integer :tag "Lines")
                               (float :tag "Height ratio"))))
  :safe 'consp
  :group 'zoom)

(defcustom zoom-ignored-major-modes nil
  "List of ignored major modes.

Selected windows using any of these major modes should not be
enlarged (only balanced)."
  :type '(repeat symbol)
  :group 'zoom)

(defcustom zoom-ignored-buffer-names nil
  "List of ignored buffer names.

Selected windows displaying any of these buffers should not be
enlarged (only balanced)."
  :type '(repeat string)
  :group 'zoom)

(defcustom zoom-ignored-buffer-name-regexps nil
  "List of ignored buffer name regexps.

Selected windows displaying buffers matching any of these regexps
should not be enlarged (only balanced)."
  :type '(repeat regexp)
  :group 'zoom)

(defcustom zoom-ignore-predicates nil
  "List of additional predicates that allow to ignore windows.

These functions are called (in order) to decide whether the
selected window should be ignored (only balanced) or not.
Predicates take no parameters and as soon as one function returns
a non-nil value, the selected window is ignored and the others
are not called."
  :type '(repeat function)
  :group 'zoom)

;;;###autoload
(define-minor-mode zoom-mode
  "Enforce a fixed and automatic balanced window layout."
  :global t
  :lighter " Z"
  :require 'zoom
  (if zoom-mode
      (zoom--on)
    (zoom--off)))

;;;###autoload
(defun zoom ()
  "Zoom the current window and balance the others."
  (interactive)
  ;; manual invocation only works when this mode is disabled
  (if zoom-mode
      (message "Window zooming is automatic (M-x zoom-mode to disable)")
    (zoom--update)))

(defun zoom--on ()
  "Enable hooks and advices and update the layout."
  (add-hook 'window-size-change-functions 'zoom--hook-handler)
  (advice-add 'select-window :after 'zoom--hook-handler)
  ;; update the layout once loaded
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (zoom--hook-handler))))

(defun zoom--off ()
  "Disable hooks and advices and evenly balance the windows."
  (remove-hook 'window-size-change-functions 'zoom--hook-handler)
  (advice-remove 'select-window 'zoom--hook-handler)
  ;; leave with a clean layout
  (dolist (frame (frame-list))
    (balance-windows frame)))

(defun zoom--hook-handler (&rest arguments)
  "Handle an update event.

ARGUMENTS is ignored."
  ;; check if should actually update
  (unless (or (not zoom-mode)
              (window-minibuffer-p)
              ;; `one-window-p' does not work well with the completion buffer
              ;; when emacsclient is used
              (frame-root-window-p (selected-window)))
    (zoom--update)))

(defun zoom--update ()
  "Update the window layout in the current frame."
  ;; temporarily disables this mode during resize to avoid infinite recursion
  ;; and enable `window-combination-resize' too ensure that other windows are
  ;; resized nicely after resizing the selected one
  (let ((zoom-mode nil)
        (window-combination-resize t))
    ;; start from a balanced layout anyway
    (balance-windows)
    ;; check if the selected window is not ignored
    (unless (zoom--window-ignored-p)
      (zoom--resize)
      (zoom--fix-scroll))))

(defun zoom--window-ignored-p ()
  "Check whether the selected window will be ignored or not."
  (or
   ;; check against the major mode
   (member major-mode zoom-ignored-major-modes)
   ;; check against the buffer name
   (member (buffer-name) zoom-ignored-buffer-names)
   ;; check against the buffer name (using a regexp)
   (catch 'ignored
     (dolist (regex zoom-ignored-buffer-name-regexps)
       (when (string-match regex (buffer-name))
         (throw 'ignored t))))
   ;; check user-defined predicates
   (catch 'ignored
     (dolist (predicate zoom-ignore-predicates)
       (when (funcall predicate)
         (throw 'ignored t))))))

(defun zoom--resize ()
  "Resize the selected window according to the user preference."
  (let ((size-hint-cons
         ;; either use the cons as is or call the custom function
         (if (functionp zoom-size) (funcall zoom-size) zoom-size)))
    (zoom--resize-one-side size-hint-cons t)
    (zoom--resize-one-side size-hint-cons nil)))

(defun zoom--resize-one-side (size-hint-cons horizontal)
  "Resize one dimension of the selected window according to the user preference.

Argument SIZE-HINT-CONS is the size hint provided by the user.

Argument HORIZONTAL determines whether the window should be
resized horizontally or vertically."
  (let* ((size-hint
          (if horizontal (car size-hint-cons) (cdr size-hint-cons)))
         (frame-size
          (if horizontal (frame-width) (frame-height)))
         ;; use the total size (including fringes, scroll bars, etc.) for ratios
         ;; and the body size for absolute values
         (window-size
          (if (floatp size-hint)
              (if horizontal (window-total-width) (window-total-height))
            (if horizontal (window-body-width) (window-body-height))))
         ;; either use an absolute value or a ratio
         (min-window-size
          (if (floatp size-hint) (round (* size-hint frame-size)) size-hint))
         ;; do not shrink the window if it is already large enough
         (desired-delta (max (- min-window-size window-size) 0))
         ;; fall back to the maximum available if the windows are too small
         (delta (window-resizable nil desired-delta horizontal)))
    ;; actually resize the window
    (window-resize nil delta horizontal)))

(defun zoom--fix-scroll ()
  "Fix the horizontal scrolling if needed."
  ;; scroll all the way to the left border
  (scroll-right (window-hscroll))
  ;; if the window is not wide enough to contain the point scroll to center
  ;; unless lines are not truncated
  (when (and truncate-lines
             (> (current-column) (- (window-body-width) hscroll-margin)))
    (scroll-left (- (current-column) (/ (window-body-width) 2)))))

(provide 'zoom)

;;; zoom.el ends here
