;;; zoom-mode.el --- Enforce a fixed and automatic balanced window layout

;;; Commentary:

;; This minor mode takes care of managing the window sizes by enforcing a fixed
;; and automatic balanced layout where the currently selected window is enlarged
;; according to `zoom-min-width' and `zoom-min-height'.

;; TODO work seamlessly on every frame
;; TODO avoid call update too often? (https://github.com/roman/golden-ratio.el/issues/57)
;; TODO catch frame resize and update
;; TODO allow to exclude major modes and buffer names?

;;; Code:

(defcustom zoom-min-width 80
  "Minimum width of the focused window in columns."
  :group 'zoom)

(defcustom zoom-min-height 24
  "Minimum height of the focused window in rows."
  :group 'zoom)

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
  (zoom--hook-handler))

(defun zoom--deregister ()
  "Disable hooks and advices and evenly balance the windows."
  (remove-hook 'window-configuration-change-hook 'zoom--hook-handler)
  (advice-remove 'select-window 'zoom--hook-handler)
  ;; leave with a clean layout
  (balance-windows))

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
