;;; zoom-mode.el --- Enforce a fixed and automatic balanced window layout

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

;;; Commentary:

;; This minor mode takes care of managing the window sizes by enforcing a fixed
;; and automatic balanced layout where the currently selected window is resized
;; according to `zoom-size' which can be either an absolute value in
;; rows/columns or a ratio between the selected window and frame size.

;;; Code:

(defgroup zoom nil
  "Enforce a fixed and automatic balanced window layout."
  :group 'windows)

(defcustom zoom-size '(80 . 24)
  "Size hint for the selected window.
Each component can be either an absolute value in rows/columns or
a ratio between the selected window and the frame size."
  :type '(cons (choice (integer :tag "Columns")
                       (float :tag "Width ratio"))
               (choice (integer :tag "Rows")
                       (float :tag "Height ratio")))
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
(defun zoom ()
  "Zoom the current window and balance the others."
  (interactive)
  ;; manual invocation only works when this mode is disabled
  (if zoom-mode
      (message "Window zooming is automatic (M-x zoom-mode to disable)")
    (zoom--update)))

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

(defun zoom--resize-one-side (horizontal)
  "Resize one dimension of the selected window according to the user preference.
Argument HORIZONTAL determines whether the window should be
resized horizontally or vertically."
  (let* ((size-hint
          (if horizontal (car zoom-size) (cdr zoom-size)))
         (window-size
          (if horizontal (window-total-width) (window-total-height)))
         (frame-size
          (if horizontal (frame-width) (frame-height)))
         ;; either use an absolute value or a ratio
         (min-window-size
          (if (floatp size-hint) (floor (* size-hint frame-size)) size-hint))
         ;; do not shrink the window if it is already large enough
         (desired-delta (max (- min-window-size window-size) 0))
         ;; fall back to the maximum available if the windows are too small
         (delta (window-resizable nil desired-delta horizontal)))
    ;; actually resize the window
    (window-resize nil delta horizontal)))

(defun zoom--resize ()
  "Resize the selected window according to the user preference."
  (zoom--resize-one-side t)
  (zoom--resize-one-side nil))

(defun zoom--fix-scroll ()
  "Fix the horizontal scrolling if needed."
  ;; scroll all the way to the left border
  (scroll-right (window-hscroll))
  ;; if the window is not wide enough to contain the point scroll to center
  ;; unless lines are not truncated
  (when (and truncate-lines
             (> (current-column) (- (window-total-width) hscroll-margin)))
    (scroll-left (- (current-column) (/ (window-total-width) 2)))))

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

(defun zoom--hook-handler (&rest arguments)
  "Handle an update event.
ARGUMENTS is ignored."
  ;; check if should actually update
  (unless (or (not zoom-mode)
              (window-minibuffer-p)
              (one-window-p))
    (zoom--update)))

(defun zoom--register ()
  "Enable hooks and advices and update the layout."
  (add-hook 'focus-in-hook 'zoom--hook-handler)
  (add-hook 'window-configuration-change-hook 'zoom--hook-handler)
  (advice-add 'select-window :after 'zoom--hook-handler)
  ;; update the layout once loaded
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (zoom--hook-handler))))

(defun zoom--deregister ()
  "Disable hooks and advices and evenly balance the windows."
  (remove-hook 'focus-in-hook 'zoom--hook-handler)
  (remove-hook 'window-configuration-change-hook 'zoom--hook-handler)
  (advice-remove 'select-window 'zoom--hook-handler)
  ;; leave with a clean layout
  (dolist (frame (frame-list))
    (balance-windows frame)))

;;;###autoload
(define-minor-mode zoom-mode
  "Enforce a fixed and automatic balanced window layout."
  :global t
  :lighter " Z"
  (if zoom-mode
      (zoom--register)
    (zoom--deregister)))

(provide 'zoom-mode)

;;; zoom-mode.el ends here
