;;------------------------------------------------------------------------------
;;delete line with no kill ring
(global-set-key (kbd "C-S-k") 'delete-line-no-kill)

;;C-x F1 : Layout
(global-set-key (kbd "C-x <f1>") 'my-two-buffer-layout)

;;C-x F2 : Layout
(global-set-key (kbd "C-x <f2>") 'my-three-buffer-layout)

;;C-x F3 : Layout
(global-set-key (kbd "C-x <f3>") 'my-five-buffer-layout)

;;eval-buffer shortcut
(global-set-key (kbd "C-x e") (lambda () (interactive) (find-file "/om/user/miker/lisp/init.el") (eval-buffer) (previous-buffer)))

;;multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this))


;;expand region
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C-;" . er/expand-region))

;;org-mode undbind
(eval-after-load "org" '(define-key org-mode-map (kbd "C-,") nil))

;;helm
(use-package helm
  :ensure t
  :config
  (setq helm-always-two-windows nil)
  (setq helm-split-window-default-side 'same)
  (defun my-helm-grep-do-git-grep (not-all)
    (interactive "P")
    (helm-grep-git-1 default-directory (null not-all)))
  :bind
  (("C-x l" . helm-mini)
   ("C-x r b" . helm-bookmarks)
   ("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-c g" . my-helm-grep-do-git-grep)))


;; clang-format
(use-package clang-format
  :ensure t
  :bind
  (("C-c f" . clang-format)))


(use-package move-text
  :ensure t
  :bind
  (("C-S-p" . move-text-up)
   ("C-S-n" . move-text-down)))

;;cc-mode bind
(eval-after-load "cc-mode"
  '(define-key c++-mode-map (kbd "C-x m") 'ff-find-other-file))


(provide 'miker-keybind)
