
# Table of Contents

1.  [Package setup](#org5ea4485)
2.  [Benchmark](#org7c5eb0e)
3.  [Basic](#org01f9580)
    1.  [Annotate fix](#orgdb216fa)
    2.  [Skip gui](#org2df3793)
    3.  [Enables show-paren-mode](#orgb29e7d8)
    4.  [Auto-save and backup files saved in TEMP](#org4fe7ca1)
    5.  [Change all prompts to y or n](#org7c20b7e)
    6.  [settings for new frames (that is new window in windows, C-x 5 2)](#orgae3b35c)
4.  [Modes](#org4874b0b)
    1.  [js2-mode](#orge9bb50f)
    2.  [nxml-mode](#org9455ec1)
    3.  [web-mode](#org8f92709)
    4.  [cc-mode](#orgea98b5a)
5.  [Functions](#orgfb50a17)
    1.  [define you own browser function (which opens \`eww' with the url)](#org7f4d185)
    2.  [Override alt-backspace](#org5aa1761)
    3.  [Delete line (without kill-ring) (Ctrl-Shift-K)](#org0c12a3b)
6.  [Look and feel](#org61b011c)
    1.  [Theme](#org7b02ddb)
        1.  [chocolate-theme](#org95f67ae)
        2.  [dracula-theme](#orgc83f03f)
    2.  [Layout](#org84b7d43)
        1.  [Add line at top of the buffer to show column length](#orga20be98)
        2.  [Fringe git-gutter settings](#orgab6fa73)
        3.  [yascroll 'no scroll bar'](#orgd45716c)
        4.  [Layout functions](#orgf2a8292)
    3.  [Input](#orgdaa3d8c)
        1.  [Insert matching delimiters](#org0aebb88)
        2.  [Indentation](#org40be427)
7.  [Packages](#org425bbe5)
    1.  [Clang-format](#orgdedaeac)
    2.  [Magit](#org5625140)
    3.  [git-timemachine](#org038373c)
    4.  [swiper](#orgc128024)
    5.  [multiple-cursors](#orga82cb6b)
    6.  [helm](#org0c6cc3e)
    7.  [expand-region](#org588acd1)
    8.  [move-text](#orgc95c359)
    9.  [which-key](#org6942e18)
8.  [Org](#org9915793)
    1.  [Unbind](#org6271373)
    2.  [org-agenda](#org5c02210)
    3.  [org-bullets](#orgf90b593)
9.  [Keybinds](#orgc0a4fbb)
    1.  [delete line with no kill ring](#org6233780)
    2.  [C-x F<n> : Layout](#orgf3e1f9c)
    3.  [end/start of buffer](#orgcb285a2)
    4.  [Switch buffers](#org706334a)
10. [Kept from old config](#org220fc60)



<a id="org5ea4485"></a>

# Package setup

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


<a id="org7c5eb0e"></a>

# Benchmark

    (use-package benchmark-init
     :ensure t)


<a id="org01f9580"></a>

# Basic


<a id="orgdb216fa"></a>

## Annotate fix

    (eval-after-load "vc-git"
      '(defun vc-git-annotate-command (file buf &optional rev)
        (let ((name (file-relative-name file)))
          (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))
    )


<a id="org2df3793"></a>

## Skip gui

    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    
    (set-variable 'tooltip-mode nil)
    (blink-cursor-mode -1)
    (column-number-mode t)
    (setq inhibit-splash-screen t)


<a id="orgb29e7d8"></a>

## Enables show-paren-mode

    (setq show-paren-delay 0)
    (show-paren-mode 1)


<a id="org4fe7ca1"></a>

## Auto-save and backup files saved in TEMP

    (setq backup-directory-alist
          `((".*" . ,"~/.backups")))
    (setq auto-save-file-name-transforms
          `((".*" ,"~/.autosaves" t)))
    (setq custom-file "~/.emacs.d/auto-custom.el")


<a id="org7c20b7e"></a>

## Change all prompts to y or n

    (fset 'yes-or-no-p 'y-or-n-p)


<a id="orgae3b35c"></a>

## settings for new frames (that is new window in windows, C-x 5 2)

    (setq default-frame-alist
          '((font . "Fira Mono")
            (vertical-scroll-bars . nil)
            (menu-bar-lines . 0)
            (left-fringe . 0)))


<a id="org4874b0b"></a>

# Modes


<a id="orge9bb50f"></a>

## js2-mode

    (use-package js2-mode
      :ensure t)
    
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))


<a id="org9455ec1"></a>

## nxml-mode

    (add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))


<a id="org8f92709"></a>

## web-mode

    (use-package web-mode
      :ensure t
      :mode ("\\.html\\'"))


<a id="orgea98b5a"></a>

## cc-mode

Switch between header and implementation file

    (eval-after-load "cc-mode"
      '(define-key c++-mode-map (kbd "C-x m") 'ff-find-other-file))


<a id="orgfb50a17"></a>

# Functions


<a id="org7f4d185"></a>

## define you own browser function (which opens \`eww' with the url)

    (defun my-browse-url-browser-function (url &rest args)
      (eww url))
    
    ;; activate your own browser function
    (setq browse-url-browser-function 'my-browse-url-browser-function)


<a id="org5aa1761"></a>

## Override alt-backspace

    (defun backward-delete-word (arg)
      (interactive "p")
      (delete-region (point) (progn (backward-word arg) (point))))
    (global-set-key (kbd "M-<backspace>") 'backward-delete-word)


<a id="org0c12a3b"></a>

## Delete line (without kill-ring) (Ctrl-Shift-K)

    (defun delete-line-no-kill ()
      (interactive)
      (delete-region
       (point)
       (save-excursion (move-end-of-line 1) (point)))
      (delete-char 1)
      (open-line 1)
    )


<a id="org61b011c"></a>

# Look and feel


<a id="org7b02ddb"></a>

## Theme


<a id="org95f67ae"></a>

### chocolate-theme

    ;;(set-background-color "wheat2")
    ;;(use-package chocolate-theme
    ;;  :ensure t
    ;;  :config
    ;;  (load-theme 'chocolate t))


<a id="orgc83f03f"></a>

### dracula-theme

    (use-package dracula-theme
      :ensure t
      :config
      (load-theme 'dracula t))
    
    (set-face-attribute 'region nil :background "#342c6b" :foreground nil)


<a id="org84b7d43"></a>

## Layout


<a id="orga20be98"></a>

### Add line at top of the buffer to show column length

    (setq-default header-line-format
                  (list " " (make-string 76 ?-) "|"))

    ;;Highlight line
    ;;(global-hl-line-mode 1)
    ;;(set-face-background hl-line-face "darkgrey")
    (set-cursor-color "#C8FF03")


<a id="orgab6fa73"></a>

### Fringe git-gutter settings

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


<a id="orgd45716c"></a>

### yascroll 'no scroll bar'

    ;; No scroll bar
    (set-face-background 'vertical-border "snow4")
    (set-face-foreground 'vertical-border (face-background 'vertical-border))
    
    (use-package yascroll
      :ensure t)
    (global-yascroll-bar-mode 1)

1.  smart-mode-line

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
    
        (setq-default show-trailing-whitespace t)


<a id="orgf2a8292"></a>

### Layout functions

1.  Split up 2 buffers

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
        
          (other-window 2)
          (add-to-list 'default-frame-alist '(fullscreen . maximized))
        )

2.  Split up 3 buffers

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
        
          (other-window 2)
          (add-to-list 'default-frame-alist '(fullscreen . maximized))
        )

3.  Split up 5 buffers

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


<a id="orgdaa3d8c"></a>

## Input


<a id="org0aebb88"></a>

### Insert matching delimiters

    (electric-pair-mode 1)
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


<a id="org40be427"></a>

### Indentation

1.  Indentation settings

        (setq-default indent-tabs-mode nil)
        (setq-default c-basic-offset 4)
        (setq-default js2-indent-level 4)
        (setq-default sgml-basic-offset 4)
        (setq-default cmake-tab-width 4)
        (setq-default nxml-child-indent 4 nxml-attribute-indent 4)
    
        ;; fix indentation.
        (c-add-style "my-cpp-style"
                     '("stroustrup"
                       (c-offsets-alist
                        (innamespace . -)
                        (inline-open . 0)
                        (access-label . -3))))
        (setq c-default-style "my-cpp-style")
    
    1.  TODO Remove my-cpp-style?


<a id="org425bbe5"></a>

# Packages


<a id="orgdedaeac"></a>

## Clang-format

    (use-package clang-format
      :ensure t
      :bind
      (("C-c f" . clang-format)))


<a id="org5625140"></a>

## Magit

    (use-package magit
      :ensure t
      :after (magit))


<a id="org038373c"></a>

## git-timemachine

    (use-package git-timemachine
      :ensure t
      :after (git-timemachine))


<a id="orgc128024"></a>

## swiper

    (use-package swiper
      :ensure t
      :bind
      ("C-c C-r" . swiper)
      )


<a id="orga82cb6b"></a>

## multiple-cursors

    (use-package multiple-cursors
      :ensure t
      :bind
      ("C->" . mc/mark-next-like-this)
      ("C-<" . mc/mark-previous-like-this)
      ("C-c C-<" . 'mc/mark-all-like-this)
    )


<a id="org0c6cc3e"></a>

## helm

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


<a id="org588acd1"></a>

## expand-region

    (use-package expand-region
      :ensure t
      :bind
      ("C-=" . er/expand-region)
      ("C-;" . er/expand-region))


<a id="orgc95c359"></a>

## move-text

    (use-package move-text
      :ensure t
      :bind
      (("C-S-p" . move-text-up)
       ("C-S-n" . move-text-down)))


<a id="org6942e18"></a>

## which-key

    (use-package which-key
      :ensure t)
    (which-key-mode)

\#+END<sub>SRC</sub>


<a id="org9915793"></a>

# Org


<a id="org6271373"></a>

## Unbind

C-, is reserved for switching buffers
C-c C-r is reserved for swiper

    (eval-after-load "org" '(define-key org-mode-map (kbd "C-,") nil))
    (eval-after-load "org" '(define-key org-mode-map (kbd "C-c C-r") nil))


<a id="org5c02210"></a>

## org-agenda

Set org-agenda files

    (setq org-agenda-files (list
    "~/.emacs.d/org/"
    "~/.emacs.d/emacs.org"))


<a id="orgf90b593"></a>

## org-bullets

    (use-package org-bullets
     :ensure t
     :init
     (setq org-bullets-bullet-list
           '("ァ" "ィ" "ゥ" "ェ" "ォ"))
     :config
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


<a id="orgc0a4fbb"></a>

# Keybinds


<a id="org6233780"></a>

## delete line with no kill ring

    (global-set-key (kbd "C-S-k") 'delete-line-no-kill)


<a id="orgf3e1f9c"></a>

## C-x F<n> : Layout

    (global-set-key (kbd "C-x <f1>") 'my-two-buffer-layout)
    
    ;;C-x F2 : Layout
    (global-set-key (kbd "C-x <f2>") 'my-three-buffer-layout)
    
    ;;C-x F3 : Layout
    (global-set-key (kbd "C-x <f3>") 'my-five-buffer-layout)


<a id="orgcb285a2"></a>

## end/start of buffer

    (global-set-key (kbd "<end>") `end-of-buffer)
    (global-set-key (kbd "<home>") `beginning-of-buffer)


<a id="org706334a"></a>

## Switch buffers

    (global-set-key (kbd "C-,")
    '(lambda()
    (interactive)
    (select-window (previous-window))))
    
    (global-set-key (kbd "C-.")
    '(lambda()
    (interactive)
    (select-window (next-window))))


<a id="org220fc60"></a>

# Kept from old config

(setq same-window-regexps '("\\\\\`\\\\\*Customiz.\*\\\\\*\\\\'" "\\\\\*cvs[az-]\*\\\\\*"))

(setq buffer-menu-buffer-font-lock-keywords
      '(("^&#x2026;.\*TAGS.\*" . font-lock-comment-face)
        ("^&#x2026;.[\*].\*" . font-lock-keyword-face)
        ("^.<code>[%]</code>.\*" . font-lock-string-face)))

(defun buffer-menu-custom-font-lock ()
  (let ((font-lock-unfontify-region-function
	 (lambda (start end)
	   (remove-text-properties start end '(font-lock-face nil)))))
    (font-lock-unfontify-buffer)
    (set (make-local-variable 'font-lock-defaults)
	 '(buffer-menu-buffer-font-lock-keywords t))
    (font-lock-fontify-buffer)))

(add-hook 'electric-buffer-menu-mode-hook 'buffer-menu-custom-font-lock)

;; auto-mode-alist
(add-to-list 'auto-mode-alist '("makefile\(" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.h\)" . c++-mode))
(add-to-list 'auto-mode-alist '("\\\\.ih$" . c++-mode))

