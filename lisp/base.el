;;;
(setq same-window-regexps '("\\`\\*Customiz.*\\*\\'" "\\*cvs[az-]*\\*"))

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))


;; settings for new frames (thatis new window in windows, C-x 5 2)
(setq default-frame-alist
      '((font . "Consolas-8.5")
	(vertical-scroll-bars . right)
	(menu-bar-lines . 0)
	(left-fringe . 0)
	(right-fringe . 0)))

;; Override alt-backspace
(defun backward-delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)


;; enables show-paren-mode
(setq show-paren-delay 0)
(show-paren-mode 1)


;; skip gui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-variable 'tooltip-mode nil)
(blink-cursor-mode -1)
(column-number-mode t)


;; ebuff-menu
;;(require 'ebuff-menu)
;;(global-set-key (kbd "C-x l") 'electric-buffer-list)

(setq buffer-menu-buffer-font-lock-keywords
      '(("^....*TAGS.*" . font-lock-comment-face)
        ("^....[*].*" . font-lock-keyword-face)
        ("^.[%].*" . font-lock-string-face)))

(defun buffer-menu-custom-font-lock ()
  (let ((font-lock-unfontify-region-function
	 (lambda (start end)
	   (remove-text-properties start end '(font-lock-face nil)))))
    (font-lock-unfontify-buffer)
    (set (make-local-variable 'font-lock-defaults)
	 '(buffer-menu-buffer-font-lock-keywords t))
    (font-lock-fontify-buffer)))

(add-hook 'electric-buffer-menu-mode-hook 'buffer-menu-custom-font-lock)


;; end/start of buffer
(global-set-key (kbd "<end>") `end-of-buffer)
(global-set-key (kbd "<home>") `beginning-of-buffer)


;; next/previous window
(defun move-to-previous-window ()
  (interactive)
  (select-window (previous-window)))

(defun move-to-next-window ()
  (interactive)
  (select-window (next-window)))

(global-set-key (kbd "C-,") 'move-to-previous-window)
(global-set-key (kbd "C-.") 'move-to-next-window)


;; auto-mode-alist
(add-to-list 'auto-mode-alist '("makefile$" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ih$" . c++-mode))


;; fix indentation.
(c-add-style "my-cpp-style"
	     '("stroustrup"
	       (c-offsets-alist
		(innamespace . -)
		(inline-open . 0)
		(access-label . -3))))

(setq c-default-style "my-cpp-style")


;; original text.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "c9321e2db48a21fc656a907e97ee85d8cd86967855bf0bed3998bcf9195c758b" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "b86c3de12c012593d19916ee6c9b1ac6f0cbb1fdf6237ead94e577867f1e9dd2" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(inhibit-startup-screen t)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
