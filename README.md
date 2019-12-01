
# Table of Contents

1.  [Package setup](#org14b46ee)
2.  [Benchmark](#org585a720)
3.  [Basic](#org0c7e9e3)
    1.  [Annotate fix](#org3f41abe)
    2.  [Skip gui](#orgefb4ab9)
    3.  [Enables show-paren-mode](#org8c1033a)
    4.  [Auto-save and backup files saved in TEMP](#orgc999f56)
    5.  [Change all prompts to y or n](#orge5ae9e1)
    6.  [settings for new frames (that is new window in windows, C-x 5 2)](#org5044f61)
4.  [Modes](#org6b8c1a3)
    1.  [js2-mode](#org2dfe099)
    2.  [nxml-mode](#org29e2898)
    3.  [web-mode](#orgc15ca20)
    4.  [cc-mode](#org7dcc81f)
5.  [Functions](#org8cf22bc)
    1.  [define you own browser function (which opens \`eww' with the url)](#orgb50ea93)
    2.  [Override alt-backspace](#org510175e)
    3.  [Delete line (without kill-ring) (Ctrl-Shift-K)](#org341de1d)
6.  [Look and feel](#orgb225f77)
    1.  [Theme](#orgb05a347)
        1.  [chocolate-theme](#orgcfe19d5)
        2.  [dracula-theme](#orgfd3d0bc)
    2.  [Layout](#orgdab0d29)
        1.  [Add line at top of the buffer to show column length](#org634c1cc)
        2.  [Fringe git-gutter settings](#orge0bb567)
        3.  [yascroll 'no scroll bar'](#org33249ef)
        4.  [smart-mode-line](#org874866d)
        5.  [Layout functions](#org0b59e13)
    3.  [Input](#org3aa3ae8)
        1.  [Insert matching delimiters](#org51ca271)
        2.  [Indentation](#orgd0bc1f1)
7.  [Packages](#orgb78643e)
    1.  [Clang-format](#orgaf2b329)
    2.  [Magit](#orge1a99ac)
    3.  [git-timemachine](#org94ba5cb)
    4.  [swiper](#org771a468)
    5.  [multiple-cursors](#orga356933)
    6.  [helm](#org176852c)
    7.  [expand-region](#org9127a45)
    8.  [move-text](#org981e36e)
    9.  [which-key](#org405fd80)
8.  [Org](#org191c1bc)
    1.  [Unbind](#orge53cd23)
    2.  [org-agenda](#org9738aca)
    3.  [org-bullets](#org64a1e22)
9.  [Keybinds](#orgded46ae)
    1.  [delete line with no kill ring](#org4f663d1)
    2.  [C-x F<n> : Layout](#org41afa40)
    3.  [end/start of buffer](#org11b61ec)
    4.  [Switch buffers](#org042b83c)
10. [Kept from old config](#org458dd34)



<a id="org14b46ee"></a>

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


<a id="org585a720"></a>

# Benchmark

    (use-package benchmark-init
     :ensure t)


<a id="org0c7e9e3"></a>

# Basic


<a id="org3f41abe"></a>

## Annotate fix

    (eval-after-load "vc-git"
      '(defun vc-git-annotate-command (file buf &optional rev)
        (let ((name (file-relative-name file)))
          (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))
    )


<a id="orgefb4ab9"></a>

## Skip gui

    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    
    (set-variable 'tooltip-mode nil)
    (blink-cursor-mode -1)
    (column-number-mode t)
    (setq inhibit-splash-screen t)


<a id="org8c1033a"></a>

## Enables show-paren-mode

    (setq show-paren-delay 0)
    (show-paren-mode 1)


<a id="orgc999f56"></a>

## Auto-save and backup files saved in TEMP

    (setq backup-directory-alist
          `((".*" . ,"~/.backups")))
    (setq auto-save-file-name-transforms
          `((".*" ,"~/.autosaves" t)))
    (setq custom-file "~/.emacs.d/auto-custom.el")


<a id="orge5ae9e1"></a>

## Change all prompts to y or n

    (fset 'yes-or-no-p 'y-or-n-p)


<a id="org5044f61"></a>

## settings for new frames (that is new window in windows, C-x 5 2)

    (setq default-frame-alist
          '((font . "Fira Mono")
            (vertical-scroll-bars . nil)
            (menu-bar-lines . 0)
            (left-fringe . 0)))


<a id="org6b8c1a3"></a>

# Modes


<a id="org2dfe099"></a>

## js2-mode

    (use-package js2-mode
      :ensure t)
    
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))


<a id="org29e2898"></a>

## nxml-mode

    (add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))


<a id="orgc15ca20"></a>

## web-mode

    (use-package web-mode
      :ensure t
      :mode ("\\.html\\'"))


<a id="org7dcc81f"></a>

## cc-mode

Switch between header and implementation file

    (eval-after-load "cc-mode"
      '(define-key c++-mode-map (kbd "C-x m") 'ff-find-other-file))


<a id="org8cf22bc"></a>

# Functions


<a id="orgb50ea93"></a>

## define you own browser function (which opens \`eww' with the url)

    (defun my-browse-url-browser-function (url &rest args)
      (eww url))
    
    ;; activate your own browser function
    (setq browse-url-browser-function 'my-browse-url-browser-function)


<a id="org510175e"></a>

## Override alt-backspace

    (defun backward-delete-word (arg)
      (interactive "p")
      (delete-region (point) (progn (backward-word arg) (point))))
    (global-set-key (kbd "M-<backspace>") 'backward-delete-word)


<a id="org341de1d"></a>

## Delete line (without kill-ring) (Ctrl-Shift-K)

    (defun delete-line-no-kill ()
      (interactive)
      (delete-region
       (point)
       (save-excursion (move-end-of-line 1) (point)))
      (delete-char 1)
      (open-line 1)
    )


<a id="orgb225f77"></a>

# Look and feel


<a id="orgb05a347"></a>

## Theme


<a id="orgcfe19d5"></a>

### chocolate-theme

    ;;(set-background-color "wheat2")
    ;;(use-package chocolate-theme
    ;;  :ensure t
    ;;  :config
    ;;  (load-theme 'chocolate t))


<a id="orgfd3d0bc"></a>

### dracula-theme

    (use-package dracula-theme
      :ensure t
      :config
      (load-theme 'dracula t))
    
    (set-face-attribute 'region nil :background "#342c6b" :foreground nil)


<a id="orgdab0d29"></a>

## Layout


<a id="org634c1cc"></a>

### Add line at top of the buffer to show column length

    (setq-default header-line-format
                  (list " " (make-string 76 ?-) "|"))

    ;;Highlight line
    ;;(global-hl-line-mode 1)
    ;;(set-face-background hl-line-face "darkgrey")
    (set-cursor-color "#C8FF03")


<a id="orge0bb567"></a>

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


<a id="org33249ef"></a>

### yascroll 'no scroll bar'

    ;; No scroll bar
    (set-face-background 'vertical-border "snow4")
    (set-face-foreground 'vertical-border (face-background 'vertical-border))
    
    (use-package yascroll
      :ensure t)
    (global-yascroll-bar-mode 1)


<a id="org874866d"></a>

### smart-mode-line

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


<a id="org0b59e13"></a>

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


<a id="org3aa3ae8"></a>

## Input


<a id="org51ca271"></a>

### Insert matching delimiters

    (electric-pair-mode 1)
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


<a id="orgd0bc1f1"></a>

### Indentation

1.  Indentation settings

        (setq-default indent-tabs-mode nil)
        (setq-default c-basic-offset 4)
        (setq-default js2-indent-level 4)
        (setq-default sgml-basic-offset 4)
        (setq-default cmake-tab-width 4)
        (setq-default nxml-child-indent 4 nxml-attribute-indent 4)


<a id="orgb78643e"></a>

# Packages


<a id="orgaf2b329"></a>

## Clang-format

    (use-package clang-format
      :ensure t
      :bind
      (("C-c f" . clang-format)))


<a id="orge1a99ac"></a>

## Magit

    (use-package magit
      :ensure t
      :after (magit))


<a id="org94ba5cb"></a>

## git-timemachine

    (use-package git-timemachine
      :ensure t
      :after (git-timemachine))


<a id="org771a468"></a>

## swiper

    (use-package swiper
      :ensure t
      :bind
      ("C-c C-r" . swiper)
      )


<a id="orga356933"></a>

## multiple-cursors

    (use-package multiple-cursors
      :ensure t
      :bind
      ("C->" . mc/mark-next-like-this)
      ("C-<" . mc/mark-previous-like-this)
      ("C-c C-<" . 'mc/mark-all-like-this)
    )


<a id="org176852c"></a>

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


<a id="org9127a45"></a>

## expand-region

    (use-package expand-region
      :ensure t
      :bind
      ("C-=" . er/expand-region)
      ("C-;" . er/expand-region))


<a id="org981e36e"></a>

## move-text

    (use-package move-text
      :ensure t
      :bind
      (("C-S-p" . move-text-up)
       ("C-S-n" . move-text-down)))


<a id="org405fd80"></a>

## which-key

    (use-package which-key
      :ensure t)
    (which-key-mode)


<a id="org191c1bc"></a>

# Org


<a id="orge53cd23"></a>

## Unbind

C-, is reserved for switching buffers
C-c C-r is reserved for swiper

    (eval-after-load "org" '(define-key org-mode-map (kbd "C-,") nil))
    (eval-after-load "org" '(define-key org-mode-map (kbd "C-c C-r") nil))


<a id="org9738aca"></a>

## org-agenda

Set org-agenda files

    (setq org-agenda-files (list
    "~/.emacs.d/org/"
    "~/.emacs.d/emacs.org"))


<a id="org64a1e22"></a>

## org-bullets

    (use-package org-bullets
     :ensure t
     :init
     (setq org-bullets-bullet-list
           '("ァ" "ィ" "ゥ" "ェ" "ォ"))
     :config
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


<a id="orgded46ae"></a>

# Keybinds


<a id="org4f663d1"></a>

## delete line with no kill ring

    (global-set-key (kbd "C-S-k") 'delete-line-no-kill)


<a id="org41afa40"></a>

## C-x F<n> : Layout

    (global-set-key (kbd "C-x <f1>") 'my-two-buffer-layout)
    
    ;;C-x F2 : Layout
    (global-set-key (kbd "C-x <f2>") 'my-three-buffer-layout)
    
    ;;C-x F3 : Layout
    (global-set-key (kbd "C-x <f3>") 'my-five-buffer-layout)


<a id="org11b61ec"></a>

## end/start of buffer

    (global-set-key (kbd "<end>") `end-of-buffer)
    (global-set-key (kbd "<home>") `beginning-of-buffer)


<a id="org042b83c"></a>

## Switch buffers

    (global-set-key (kbd "C-,")
    '(lambda()
    (interactive)
    (select-window (previous-window))))
    
    (global-set-key (kbd "C-.")
    '(lambda()
    (interactive)
    (select-window (next-window))))


<a id="org458dd34"></a>

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

