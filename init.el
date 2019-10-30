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
(use-package js2-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))


;;------------------------------------------------------------------------------
;; Theme
;;(set-background-color "wheat2")
(use-package chocolate-theme
  :ensure t
  :config
  (load-theme 'chocolate t))

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
(setq custom-file "~/.emacs.d/auto-custom.el")


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
;; clang-format
(use-package clang-format
  :ensure t
  :bind
  (("C-c f" . clang-format)))


;;------------------------------------------------------------------------------
;; magit
(use-package magit
  :ensure t
  :after (magit))


;;------------------------------------------------------------------------------
;; git-timemachine
(use-package git-timemachine
  :ensure t
  :after (git-timemachine))


;;------------------------------------------------------------------------------
;; web-mode
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"))

;;------------------------------------------------------------------------------
;; org-bullets
(use-package org-bullets
 :ensure t
 :init
 (setq org-bullets-bullet-list
       '("ァ" "ィ" "ゥ" "ェ" "ォ"))
 :config
 (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;------------------------------------------------------------------------------
;; swiper-helm
(use-package swiper
  :ensure t
  :bind
  ("C-c C-r" . swiper)
  )


;;------------------------------------------------------------------------------
;; indentation settings
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default js2-indent-level 4)
(setq-default sgml-basic-offset 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END my stuff /miker
