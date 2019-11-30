
# Table of Contents

1.  [Basic](#org5821dbb)
    1.  [Annotate fix](#orgd5b3321)
    2.  [Skip gui](#orgb828589)
    3.  [Enables show-paren-mode](#orge25c298)
    4.  [Auto-save and backup files saved in TEMP](#org4c7c745)
    5.  [Change all prompts to y or n](#orgb31f39b)
    6.  [settings for new frames (that is new window in windows, C-x 5 2)](#orgb90939b)
2.  [Package setup](#org85de67c)
3.  [Benchmark](#orgfd10a64)
4.  [Modes](#orged07d56)
    1.  [js2-mode](#org7e71a9e)
    2.  [nxml-mode](#orge7597d7)
    3.  [web-mode](#org2d369d1)
    4.  [cc-mode](#orgc50b369)
5.  [Functions](#org0cf0f0d)
    1.  [define you own browser function (which opens \`eww' with the url)](#orgabda01d)
    2.  [Override alt-backspace](#orga85d645)
    3.  [Delete line (without kill-ring) (Ctrl-Shift-K)](#orgfb4bdf8)
6.  [Look and feel](#org1285d50)
    1.  [Theme](#org16f716e)
        1.  [chocolate-theme](#org73994b3)
        2.  [dracula-theme](#org6f1d64e)
    2.  [Layout](#org756c4bc)
        1.  [Add line at top of the buffer to show column length](#org9f8c116)
        2.  [Fringe git-gutter settings](#orged6fe92)
        3.  [yascroll 'no scroll bar'](#org580de31)
        4.  [Layout functions](#org02a271d)
    3.  [Input](#org1ee4c3f)
        1.  [Insert matching delimiters](#org809577f)
        2.  [Indentation](#org1536766)
7.  [Packages](#org37cc256)
    1.  [Clang-format](#org99d2dac)
    2.  [Magit](#org3185931)
    3.  [git-timemachine](#orgf9dd5e2)
    4.  [swiper](#org8d61497)
    5.  [multiple-cursors](#org65f8429)
    6.  [helm](#orgad8a64f)
    7.  [expand-region](#org2bd4f80)
    8.  [move-text](#org90c7e32)
8.  [Org](#org32a2537)
    1.  [org-bullets package](#org31d04c1)
9.  [Keybinds](#org7c9acd9)
    1.  [delete line with no kill ring](#org1ad068a)
    2.  [C-x F<n> : Layout](#org402a7bb)
    3.  [end/start of buffer](#org2e8cf1c)
    4.  [Switch buffers](#org7dbe53b)
10. [Kept from old config](#orgb1f988f)



<a id="org5821dbb"></a>

# Basic


<a id="orgd5b3321"></a>

## Annotate fix

    (eval-after-load "vc-git"
      '(defun vc-git-annotate-command (file buf &optional rev)
        (let ((name (file-relative-name file)))
          (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))
    )


<a id="orgb828589"></a>

## Skip gui

    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    
    (set-variable 'tooltip-mode nil)
    (blink-cursor-mode -1)
    (column-number-mode t)
    (setq inhibit-splash-screen t)


<a id="orge25c298"></a>

## Enables show-paren-mode

    (setq show-paren-delay 0)
    (show-paren-mode 1)


<a id="org4c7c745"></a>

## Auto-save and backup files saved in TEMP

    (setq backup-directory-alist
          `((".*" . ,"~/.backups")))
    (setq auto-save-file-name-transforms
          `((".*" ,"~/.autosaves" t)))
    (setq custom-file "~/.emacs.d/auto-custom.el")


<a id="orgb31f39b"></a>

## Change all prompts to y or n

    (fset 'yes-or-no-p 'y-or-n-p)


<a id="orgb90939b"></a>

## settings for new frames (that is new window in windows, C-x 5 2)

    (setq default-frame-alist
          '((font . "Fira Mono")
            (vertical-scroll-bars . nil)
            (menu-bar-lines . 0)
            (left-fringe . 0)))


<a id="org85de67c"></a>

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


<a id="orgfd10a64"></a>

# Benchmark

    (use-package benchmark-init
     :ensure t)


<a id="orged07d56"></a>

# Modes


<a id="org7e71a9e"></a>

## js2-mode

    (use-package js2-mode
      :ensure t)
    
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))


<a id="orge7597d7"></a>

## nxml-mode

    (add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))


<a id="org2d369d1"></a>

## web-mode

    (use-package web-mode
      :ensure t
      :mode ("\\.html\\'"))


<a id="orgc50b369"></a>

## cc-mode

Switch between header and implementation file

    (eval-after-load "cc-mode"
      '(define-key c++-mode-map (kbd "C-x m") 'ff-find-other-file))


<a id="org0cf0f0d"></a>

# Functions


<a id="orgabda01d"></a>

## define you own browser function (which opens \`eww' with the url)

    (defun my-browse-url-browser-function (url &rest args)
      (eww url))
    
    ;; activate your own browser function
    (setq browse-url-browser-function 'my-browse-url-browser-function)


<a id="orga85d645"></a>

## Override alt-backspace

    (defun backward-delete-word (arg)
      (interactive "p")
      (delete-region (point) (progn (backward-word arg) (point))))
    (global-set-key (kbd "M-<backspace>") 'backward-delete-word)


<a id="orgfb4bdf8"></a>

## Delete line (without kill-ring) (Ctrl-Shift-K)

    (defun delete-line-no-kill ()
      (interactive)
      (delete-region
       (point)
       (save-excursion (move-end-of-line 1) (point)))
      (delete-char 1)
      (open-line 1)
    )


<a id="org1285d50"></a>

# Look and feel


<a id="org16f716e"></a>

## Theme


<a id="org73994b3"></a>

### chocolate-theme

    ;;(set-background-color "wheat2")
    ;;(use-package chocolate-theme
    ;;  :ensure t
    ;;  :config
    ;;  (load-theme 'chocolate t))


<a id="org6f1d64e"></a>

### dracula-theme

    (use-package dracula-theme
      :ensure t
      :config
      (load-theme 'dracula t))
    
    (set-face-attribute 'region nil :background "#342c6b" :foreground nil)


<a id="org756c4bc"></a>

## Layout


<a id="org9f8c116"></a>

### Add line at top of the buffer to show column length

    (setq-default header-line-format
                  (list " " (make-string 76 ?-) "|"))

    ;;Highlight line
    ;;(global-hl-line-mode 1)
    ;;(set-face-background hl-line-face "darkgrey")
    (set-cursor-color "#C8FF03")


<a id="orged6fe92"></a>

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


<a id="org580de31"></a>

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


<a id="org02a271d"></a>

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
        
          (other-window 3)
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
        
          (other-window 3)
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


<a id="org1ee4c3f"></a>

## Input


<a id="org809577f"></a>

### Insert matching delimiters

    (electric-pair-mode 1)
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


<a id="org1536766"></a>

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
    
    1.  TODO Remove my-coo-style?


<a id="org37cc256"></a>

# Packages


<a id="org99d2dac"></a>

## Clang-format

    (use-package clang-format
      :ensure t
      :bind
      (("C-c f" . clang-format)))


<a id="org3185931"></a>

## Magit

    (use-package magit
      :ensure t
      :after (magit))


<a id="orgf9dd5e2"></a>

## git-timemachine

    (use-package git-timemachine
      :ensure t
      :after (git-timemachine))


<a id="org8d61497"></a>

## swiper

    (use-package swiper
      :ensure t
      :bind
      ("C-c C-r" . swiper)
      )


<a id="org65f8429"></a>

## multiple-cursors

    (use-package multiple-cursors
      :ensure t
      :bind
      ("C->" . mc/mark-next-like-this)
      ("C-<" . mc/mark-previous-like-this)
      ("C-c C-<" . 'mc/mark-all-like-this)
    )


<a id="orgad8a64f"></a>

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


<a id="org2bd4f80"></a>

## expand-region

    (use-package expand-region
      :ensure t
      :bind
      ("C-=" . er/expand-region)
      ("C-;" . er/expand-region))


<a id="org90c7e32"></a>

## move-text

    (use-package move-text
      :ensure t
      :bind
      (("C-S-p" . move-text-up)
       ("C-S-n" . move-text-down)))


<a id="org32a2537"></a>

# Org

C-, is reserved for switching buffers

    ;;org-mode undbind
    (eval-after-load "org" '(define-key org-mode-map (kbd "C-,") nil))


<a id="org31d04c1"></a>

## org-bullets package

    (use-package org-bullets
     :ensure t
     :init
     (setq org-bullets-bullet-list
           '("ァ" "ィ" "ゥ" "ェ" "ォ"))
     :config
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


<a id="org7c9acd9"></a>

# Keybinds


<a id="org1ad068a"></a>

## delete line with no kill ring

    (global-set-key (kbd "C-S-k") 'delete-line-no-kill)


<a id="org402a7bb"></a>

## C-x F<n> : Layout

    (global-set-key (kbd "C-x <f1>") 'my-two-buffer-layout)
    
    ;;C-x F2 : Layout
    (global-set-key (kbd "C-x <f2>") 'my-three-buffer-layout)
    
    ;;C-x F3 : Layout
    (global-set-key (kbd "C-x <f3>") 'my-five-buffer-layout)


<a id="org2e8cf1c"></a>

## end/start of buffer

    (global-set-key (kbd "<end>") `end-of-buffer)
    (global-set-key (kbd "<home>") `beginning-of-buffer)


<a id="org7dbe53b"></a>

## Switch buffers

    (global-set-key (kbd "C-,")
    '(lambda()
    (interactive)
    (select-window (previous-window))))
    
    (global-set-key (kbd "C-.")
    '(lambda()
    (interactive)
    (select-window (next-window))))


<a id="orgb1f988f"></a>

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

