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
;; and automatic balanced layout where the currently selected window is enlarged
;; according to `zoom-min-width' and `zoom-min-height'.

;; TODO avoid call update too often? (https://github.com/roman/golden-ratio.el/issues/57)
;; TODO catch frame resize and update
;; TODO allow to exclude major modes and buffer names?

;;; Code:

(defgroup zoom nil
  "Enforce a fixed and automatic balanced window layout."
  :group 'windows)

(defcustom zoom-min-width 80
  "Minimum width of the focused window in columns."
  :group 'zoom
  :type 'integer)

(defcustom zoom-min-height 24
  "Minimum height of the focused window in rows."
  :group 'zoom
  :type 'integer)

;;;###autoload
(defun zoom ()
  "Zoom the current window and balance the others."
  (interactive)
  ;; manual invocation only works when this mode is disabled
  (if zoom-mode
      (message "Window zooming is automatic (M-x zoom-mode to disable)")
    (zoom--update)))

(defun zoom--update ()
  "Update the window layout in the current frame."
  ;; temporarily disables this mode during resize to avoid infinite recursion
  ;; and enable `window-combination-resize' too ensure that other windows are
  ;; resized nicely after resizing the focused one
  (let ((zoom-mode nil)
        (window-combination-resize t))
    ;; start from a balanced layout
    (balance-windows)
    ;; then resize the focused window
    (let ((delta-width (max (- zoom-min-width (window-total-width)) 0))
          (delta-height (max (- zoom-min-height (window-total-height)) 0)))
      ;; fall back to the maximum available if the windows are too small
      (window-resize nil (window-resizable nil delta-width t) t)
      (window-resize nil (window-resizable nil delta-height nil) nil))
    ;; scroll all the way to the left border (if the window is wide enough to
    ;; contain it) otherwise scroll to center the point
    (scroll-right (window-hscroll))
    (if (> (current-column) (- (window-total-width) hscroll-margin))
        (scroll-left (- (current-column) (/ (window-total-width) 2))))))

(defun zoom--hook-handler (&rest ignore)
  "Handle an update event."
  ;; check if should actually update
  (unless (or (not zoom-mode)
              (window-minibuffer-p)
              (one-window-p))
    (zoom--update)))

(defun zoom--register ()
  "Enable hooks and advices and update the layout."
  (add-hook 'window-configuration-change-hook 'zoom--hook-handler)
  (advice-add 'select-window :after 'zoom--hook-handler)
  ;; update the layout once loaded
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (zoom--hook-handler))))

(defun zoom--deregister ()
  "Disable hooks and advices and evenly balance the windows."
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
