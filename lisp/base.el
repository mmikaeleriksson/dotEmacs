;;;
(setq same-window-regexps '("\\`\\*Customiz.*\\*\\'" "\\*cvs[az-]*\\*"))

;; settings for new frames (thatis new window in windows, C-x 5 2)
(setq default-frame-alist
      '((font . "Roboto Mono-9")
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

(setq inhibit-splash-screen t)

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


;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)


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
