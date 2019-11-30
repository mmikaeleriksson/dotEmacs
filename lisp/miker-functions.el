;;------------------------------------------------------------------------------
;;Split up 2 buffers
(defun my-two-buffer-layout ()
  (interactive)
  (delete-other-windows)

  (defadvice split-window-horizontally (after rebalance-windows activate)
    (balance-windows))

  (ad-activate 'split-window-horizontally)
  (switch-to-buffer
   "*scratch*")

  (split-window-horizontally) ;; -> |
  (next-multiframe-window)
  (switch-to-buffer
   "*scratch*")

  (other-window 3)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
)


;;------------------------------------------------------------------------------
;;Split up 3 buffers
(defun my-three-buffer-layout ()
  (interactive)
  (delete-other-windows)

  (defadvice split-window-horizontally (after rebalance-windows activate)
    (balance-windows))

  (ad-activate 'split-window-horizontally)
  (switch-to-buffer
   "*scratch*")

  (split-window-horizontally) ;; -> |
  (next-multiframe-window)
  (switch-to-buffer
   "*scratch*")

  (split-window-horizontally) ;; -> |
  (next-multiframe-window)
  (switch-to-buffer
   "*scratch*")

  (other-window 3)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
)


;;------------------------------------------------------------------------------
;;Split up 5 buffers
(defun my-five-buffer-layout ()
  (interactive)
  (delete-other-windows)

  (defadvice split-window-horizontally (after rebalance-windows activate)
    (balance-windows))

  (ad-activate 'split-window-horizontally)
  (switch-to-buffer
   "*scratch*")

  (split-window-horizontally) ;; -> |
  (next-multiframe-window)
  (switch-to-buffer
   "*scratch*")

  (split-window-horizontally) ;; -> |
  (next-multiframe-window)
  (switch-to-buffer
   "*scratch*")

  (split-window-horizontally) ;; -> |
  (next-multiframe-window)
  (switch-to-buffer
   "*scratch*")

  (split-window-horizontally) ;; -> |
  (next-multiframe-window)
  (switch-to-buffer
   "*scratch*")

  (other-window 3)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
)


;;------------------------------------------------------------------------------
;;Delete line (without kill-ring) (Ctrl-Shift-K
(defun delete-line-no-kill ()
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1)
  (open-line 1)
)


(provide 'miker-functions)
