;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BEGIN my stuff /miker

(require 'package)
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("stable-melpa" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(setq package-enable-at-startup nil)

(unless package-archive-contents
  (message "Refreshing package archives...")
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (message "`use-package' not found.  Installing...")
  (package-install 'use-package))

(require 'use-package)
(setq use-package-minimum-reported-time 0
      use-package-verbose t)

;;------------------------------------------------------------------------------
;; benchmark-init
(use-package benchmark-init
  :ensure t)


;;------------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "miker-functions.el")
(load "miker-keybind.el")


;;------------------------------------------------------------------------------
;;JS2 Mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))



;;------------------------------------------------------------------------------
;; Theme
;;(set-background-color "wheat2")
(load "liso-theme/liso-theme.el")


;;------------------------------------------------------------------------------
;; Add line at top of the buffer to show column length
(setq-default header-line-format
              (list " " (make-string 76 ?-) "|"))


;;------------------------------------------------------------------------------
;;Annotate fix
(eval-after-load "vc-git"
  '(defun vc-git-annotate-command (file buf &optional rev)
    (let ((name (file-relative-name file)))
      (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))
)


;;------------------------------------------------------------------------------
;;Highlight line
;;(global-hl-line-mode 1)
;;(set-face-background hl-line-face "darkgrey")
(set-cursor-color "#C8FF03")



;;------------------------------------------------------------------------------
;;Auto-save and backup files saved in TEMP
(setq backup-directory-alist
          `((".*" . ,"~/.backups")))
    (setq auto-save-file-name-transforms
          `((".*" ,"~/.autosaves" t)))


;;------------------------------------------------------------------------------
;;Fringe git-gutter settings
(use-package git-gutter
  :ensure t)
(use-package fringe-helper
  :ensure t)
(use-package git-gutter-fringe
  :ensure t)

(set-face-attribute 'fringe nil :background "darkgrey" :foreground "darkgrey")
(setq-default right-fringe-width 15)
(setq git-gutter-fr:side 'right-fringe)
;foregound
(set-face-foreground 'git-gutter-fr:modified "orange")
(set-face-foreground 'git-gutter-fr:added "dark sea green")
(set-face-foreground 'git-gutter-fr:deleted "tomato1")
;background
(set-face-background 'git-gutter-fr:added  "SystemGrayText")
(set-face-background 'git-gutter-fr:deleted  "SystemGrayText")
(set-face-background 'git-gutter-fr:modified  "SystemGrayText")
(global-git-gutter-mode)


;;------------------------------------------------------------------------------
;; No scroll bar
(scroll-bar-mode -1)
(set-face-background 'vertical-border "snow4")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

(use-package yascroll
  :ensure t)
(global-yascroll-bar-mode 1)

(load "base.el")


;;------------------------------------------------------------------------------
;; define you own browser function (which opens `eww' with the url)
(defun my-browse-url-browser-function (url &rest args)
  (eww url))


;; activate your own browser function
(setq browse-url-browser-function 'my-browse-url-browser-function)


;;------------------------------------------------------------------------------
;; Smart mode line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme nil)
  (setq sml/directory-truncation-string ".../")
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/name-width 40)
  (setq sml/mode-width 40))
(sml/setup)


;;------------------------------------------------------------------------------
;;Insert matching delimiters
(electric-pair-mode 1)

(defun my-inhibit-electric-pair-mode (char)
  (minibufferp))

(setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode)


;;------------------------------------------------------------------------------
;;Show trailing whitespace
(setq-default show-trailing-whitespace t)


;;------------------------------------------------------------------------------
;; magit
(use-package magit
  :after (magit))


;;------------------------------------------------------------------------------
;; git-timemachine
(use-package git-timemachine
  :after (git-timemachine))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END my stuff /miker
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "c9321e2db48a21fc656a907e97ee85d8cd86967855bf0bed3998bcf9195c758b" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "b86c3de12c012593d19916ee6c9b1ac6f0cbb1fdf6237ead94e577867f1e9dd2" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (multiple-cursors yascroll use-package smart-mode-line move-text helm git-gutter-fringe clang-format))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
