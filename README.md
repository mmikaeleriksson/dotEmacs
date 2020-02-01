
# Table of Contents

1.  [Package setup](#org655d79a)
2.  [Benchmark](#org45c0a2d)
3.  [Basic](#orge304978)
    1.  [Annotate fix](#org2571a55)
    2.  [Skip gui](#orgeaff5bf)
    3.  [Enables show-paren-mode](#org76a1a9d)
    4.  [Auto-save and backup files saved in TEMP](#orgd547ad0)
    5.  [Change all prompts to y or n](#orgf4bb5ce)
    6.  [settings for new frames (that is new window in windows, C-x 5 2)](#org204f2d6)
4.  [Modes](#org8617fd9)
    1.  [js2-mode](#org5cd5726)
    2.  [nxml-mode](#org9281fcd)
    3.  [web-mode](#org08ae9c1)
    4.  [cc-mode](#orgfc4276f)
5.  [Functions](#org2e6bfe0)
    1.  [Override alt-backspace](#orgfbe2137)
    2.  [Delete line (without kill-ring) (Ctrl-Shift-K)](#orgfb2279d)
    3.  [isearch-query-replace-symbol-at-point (M-s %)](#org78af8b4)
6.  [Look and feel](#org4f1423e)
    1.  [Theme](#org29ce1ea)
        1.  [chocolate-theme](#org5fa817a)
        2.  [dracula-theme](#orga3a9b68)
    2.  [Layout](#org9cd1bbd)
        1.  [Add line at top of the buffer to show column length](#org8789a8d)
        2.  [Fringe git-gutter settings](#orgfc8485f)
        3.  [yascroll 'no scroll bar'](#orgdae04b9)
        4.  [smart-mode-line](#org3587074)
        5.  [Trailing whitespaces](#org7212be9)
        6.  [Layout functions](#org7e5364a)
        7.  [display-time-mode](#org0e40f28)
    3.  [Input](#org9fb5407)
        1.  [Insert matching delimiters](#org9da85e1)
        2.  [Indentation](#org4f55f52)
        3.  [I-search](#org2c4e74d)
7.  [Packages](#org000dd23)
    1.  [Clang-format](#orge18429b)
    2.  [Magit](#org066db09)
    3.  [git-timemachine](#org4da296d)
    4.  [swiper](#org8363317)
    5.  [multiple-cursors](#org3d0b245)
    6.  [helm](#org57630ab)
    7.  [expand-region](#org5d30d6d)
    8.  [move-text](#org3f82266)
    9.  [which-key](#orga533d0e)
    10. [doom-modeline](#org64f3434)
    11. [minimap](#org8716b23)
8.  [EXWM](#org458cd3d)
    1.  [Add wm](#orga481f93)
    2.  [exwm-package](#org005b134)
    3.  [dmenu](#orgd3d2c08)
    4.  [systemtray](#orgc845cf0)
    5.  [randr](#org18dba9f)
    6.  [enable XF86](#orgb8a4fa3)
    7.  [keybinds](#org7f3f3d4)
    8.  [volume](#orgec19f9b)
    9.  [default browser](#org1c89353)
9.  [Org](#org6cf663c)
    1.  [Unbind](#orga172a20)
    2.  [org-todo-keywords](#orgd02fde5)
    3.  [org-super-agenda](#org33408b8)
    4.  [org-bullets](#org59237e8)
    5.  [tags](#org9564067)
    6.  [custom](#org038913c)
10. [Keybinds](#org37b10e4)
    1.  [end/start of buffer](#org24bd541)
    2.  [Switch buffers](#org4f413c1)



<a id="org655d79a"></a>

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


<a id="org45c0a2d"></a>

# Benchmark

    (use-package benchmark-init
     :ensure t)


<a id="orge304978"></a>

# Basic


<a id="org2571a55"></a>

## Annotate fix

    (eval-after-load "vc-git"
      '(defun vc-git-annotate-command (file buf &optional rev)
        (let ((name (file-relative-name file)))
          (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))
    )


<a id="orgeaff5bf"></a>

## Skip gui

    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    
    (set-variable 'tooltip-mode nil)
    (blink-cursor-mode -1)
    (column-number-mode t)
    (setq inhibit-splash-screen t)


<a id="org76a1a9d"></a>

## Enables show-paren-mode

    (setq show-paren-delay 0)
    (show-paren-mode 1)


<a id="orgd547ad0"></a>

## Auto-save and backup files saved in TEMP

    (setq backup-directory-alist
          `((".*" . ,"~/.saves")))
    (setq custom-file "~/.emacs.d/auto-custom.el")


<a id="orgf4bb5ce"></a>

## Change all prompts to y or n

    (fset 'yes-or-no-p 'y-or-n-p)


<a id="org204f2d6"></a>

## settings for new frames (that is new window in windows, C-x 5 2)

    (setq default-frame-alist
          '((font . "Fira Mono")
            (vertical-scroll-bars . nil)
            (menu-bar-lines . 0)
            (left-fringe . 0)))


<a id="org8617fd9"></a>

# Modes


<a id="org5cd5726"></a>

## js2-mode

    (use-package js2-mode
      :ensure t)
    
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))


<a id="org9281fcd"></a>

## nxml-mode

    (add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))


<a id="org08ae9c1"></a>

## web-mode

    (use-package web-mode
      :ensure t
      :mode ("\\.html\\'"))


<a id="orgfc4276f"></a>

## cc-mode

Switch between header and implementation file

    (eval-after-load "cc-mode"
      '(define-key c++-mode-map (kbd "C-x m") 'ff-find-other-file))


<a id="org2e6bfe0"></a>

# Functions


<a id="orgfbe2137"></a>

## Override alt-backspace

    (defun miker/backward-delete-word (arg)
      (interactive "p")
      (delete-region (point) (progn (backward-word arg) (point))))
    (global-set-key (kbd "M-<backspace>") 'miker/backward-delete-word)


<a id="orgfb2279d"></a>

## Delete line (without kill-ring) (Ctrl-Shift-K)

    (defun miker/delete-line-no-kill ()
      (interactive)
      (delete-region
       (point)
       (save-excursion (move-end-of-line 1) (point)))
      (delete-char 1)
      (open-line 1)
    )
    (global-set-key (kbd "C-S-k") 'miker/delete-line-no-kill)


<a id="org78af8b4"></a>

## isearch-query-replace-symbol-at-point (M-s %)

    (defun miker/isearch-query-replace-symbol-at-point ()
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp)
    )
    (global-set-key (kbd "M-s %") 'miker/isearch-query-replace-symbol-at-point)


<a id="org4f1423e"></a>

# Look and feel


<a id="org29ce1ea"></a>

## Theme


<a id="org5fa817a"></a>

### chocolate-theme

    ;;(set-background-color "wheat2")
    ;;(use-package chocolate-theme
    ;;  :ensure t
    ;;  :config
    ;;  (load-theme 'chocolate t))


<a id="orga3a9b68"></a>

### dracula-theme

    (use-package dracula-theme
      :ensure t
      :config
      (load-theme 'dracula t))
    
    (set-face-attribute 'region nil :background "#342c6b" :foreground nil)


<a id="org9cd1bbd"></a>

## Layout


<a id="org8789a8d"></a>

### Add line at top of the buffer to show column length

    (setq-default header-line-format
                  (list " " (make-string 76 ?-) "|"))

    ;;Highlight line
    ;;(global-hl-line-mode 1)
    ;;(set-face-background hl-line-face "darkgrey")
    (set-cursor-color "#C8FF03")


<a id="orgfc8485f"></a>

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


<a id="orgdae04b9"></a>

### yascroll 'no scroll bar'

    ;; No scroll bar
    (set-face-background 'vertical-border "snow4")
    (set-face-foreground 'vertical-border (face-background 'vertical-border))
    
    (use-package yascroll
      :ensure t)
    (global-yascroll-bar-mode 1)


<a id="org3587074"></a>

### smart-mode-line

    ;(use-package smart-mode-line
    ;  :ensure t
    ;  :config
    ;  (setq sml/theme nil)
    ;  (setq sml/directory-truncation-string ".../")
    ;  (setq sml/shorten-directory t)
    ;  (setq sml/shorten-modes t)
    ;  (setq sml/name-width 40)
    ;  (setq sml/mode-width 40))
    ;(sml/setup)


<a id="org7212be9"></a>

### Trailing whitespaces

    (setq-default show-trailing-whitespace t)


<a id="org7e5364a"></a>

### Layout functions

1.  Split up 2 buffers

        (defun miker/two-buffer-layout ()
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
        (global-set-key (kbd "C-x <f1>") 'miker/two-buffer-layout)

2.  Split up 3 buffers

        (defun miker/three-buffer-layout ()
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
        (global-set-key (kbd "C-x <f2>") 'miker/three-buffer-layout)

3.  Split up 5 buffers

        (defun miker/five-buffer-layout ()
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
        (global-set-key (kbd "C-x <f3>") 'miker/five-buffer-layout)


<a id="org0e40f28"></a>

### display-time-mode

    (setq display-time-24hr-format t)
    (setq display-time-format "(%H:%M %e/%m)")
    (display-time-mode 1)


<a id="org9fb5407"></a>

## Input


<a id="org9da85e1"></a>

### Insert matching delimiters

    (electric-pair-mode 1)
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


<a id="org4f55f52"></a>

### Indentation

    (setq-default indent-tabs-mode nil)
    (setq-default c-basic-offset 4)
    (setq-default js2-indent-level 4)
    (setq-default sgml-basic-offset 4)
    (setq-default cmake-tab-width 4)
    (setq-default nxml-child-indent 4 nxml-attribute-indent 4)


<a id="org2c4e74d"></a>

### I-search

    (setq search-whitespace-regexp ".*?")


<a id="org000dd23"></a>

# Packages


<a id="orge18429b"></a>

## Clang-format

    (use-package clang-format
      :ensure t
      :bind
      (("C-c f" . clang-format)))


<a id="org066db09"></a>

## Magit

    (use-package magit
      :ensure t
      :after (magit))


<a id="org4da296d"></a>

## git-timemachine

    (use-package git-timemachine
      :ensure t
      :after (git-timemachine))


<a id="org8363317"></a>

## swiper

    (use-package swiper
      :ensure t
      :bind
      ("C-c C-r" . swiper)
      )


<a id="org3d0b245"></a>

## multiple-cursors

    (use-package multiple-cursors
      :ensure t
      :bind
      ("C->" . mc/mark-next-like-this)
      ("C-<" . mc/mark-previous-like-this)
      ("C-c C-<" . 'mc/mark-all-like-this)
    )


<a id="org57630ab"></a>

## helm

    (use-package helm
      :ensure t
      :config
      (setq helm-always-two-windows nil)
      (setq helm-split-window-default-side 'same)
      (defun miker/helm-grep-do-git-grep (not-all)
        (interactive "P")
        (helm-grep-git-1 default-directory (null not-all)))
      :bind
      (("C-x l" . helm-mini)
       ("C-x r b" . helm-bookmarks)
       ("C-x C-f" . helm-find-files)
       ("M-x" . helm-M-x)
       ("M-y" . helm-show-kill-ring)
       ("C-c g" . miker/helm-grep-do-git-grep)))


<a id="org5d30d6d"></a>

## expand-region

    (use-package expand-region
      :ensure t
      :bind
      ("C-=" . er/expand-region)
      ("C-;" . er/expand-region))


<a id="org3f82266"></a>

## move-text

    (use-package move-text
      :ensure t
      :bind
      (("C-S-p" . move-text-up)
       ("C-S-n" . move-text-down)))


<a id="orga533d0e"></a>

## which-key

    (use-package which-key
      :ensure t)
    (which-key-mode)


<a id="org64f3434"></a>

## doom-modeline

    (use-package all-the-icons)
    (use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-init)
    )


<a id="org8716b23"></a>

## minimap

    (use-package minimap
      :ensure t
      :custom
      (minimap-window-location 'right)
      :config
      ;;dracula color
      (set-face-attribute 'minimap-active-region-background nil :background "#342c6b")
    )


<a id="org458cd3d"></a>

# EXWM

tangle yes/no to enable/disable EXWM


<a id="orga481f93"></a>

## Add wm

    cd /usr/share/xsessions
    touch emacs.desktop

Add to emacs.desktop:

    [Desktop Entry]
    Name=EXWM
    Comment=Emacs window manager
    Exec=emacs
    Type=Application


<a id="org005b134"></a>

## exwm-package

    (use-package exwm
      :ensure t
      :config
      (require 'exwm-config)
      (exwm-config-default)
    )


<a id="orgd3d2c08"></a>

## dmenu

    (use-package dmenu
      :ensure t
      :bind
      ("s-SPC" . 'dmenu))


<a id="orgc845cf0"></a>

## systemtray

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)


<a id="org18dba9f"></a>

## randr

    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist
    '(0 "DVI-D-0" 1 "DP-2"))
    (exwm-randr-enable)

Added in *home/<user>*.profile :
xrandr &#x2013;output DP-2 &#x2013;primary &#x2013;mode 2560x1440 &#x2013;rate 143.96
xrandr &#x2013;output DVI-D-0 &#x2013;mode 1920x1080 &#x2013;rotate right &#x2013;left-of DP-2

Run xrandr in term to see current settings


<a id="orgb8a4fa3"></a>

## enable XF86

    (dolist (k '(XF86AudioLowerVolume
                 XF86AudioRaiseVolume
                 XF86PowerOff
                 XF86AudioMute
                 XF86AudioPlay
                 XF86AudioStop
                 XF86AudioPrev
                 XF86AudioNext
                 XF86ScreenSaver
                 XF68Back
                 XF86Forward
                 Scroll_Lock
                 print)))


<a id="org7f3f3d4"></a>

## keybinds

    (global-set-key (kbd "s-k") 'exwm-workspace-delete)
    (global-set-key (kbd "s-w") 'exwm-workspace-swap)
    
    (global-set-key (kbd "<XF86ScreenSaver>") 'miker/launch-lock-screen)
    (global-set-key (kbd "<XF86PowerOff>") 'miker/launch-shutdown)


<a id="orgec19f9b"></a>

## volume

    (defconst volumeModifier "4")
    
    (defun audio/mute ()
      (interactive)
      (start-process "audio-mute" nil "pulsemixer" "--toggle-mute"))
    
    (defun audio/raise-volume ()
      (interactive)
      (start-process "raise-volume" nil "pulsemixer" "--change-volume" (concat "+" volumeModifier)))
    
    (defun audio/lower-volume ()
      (interactive)
      (start-process "lower-volume" nil "pulsemixer" "--change-volume" (concat "-" volumeModifier)))
    
    (global-set-key (kbd "<XF86AudioMute>") 'audio/mute)
    (global-set-key (kbd "<XF86AudioRaiseVolume>") 'audio/raise-volume)
    (global-set-key (kbd "<XF86AudioLowerVolume>") 'audio/lower-volume)


<a id="org1c89353"></a>

## default browser

    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "firefox")


<a id="org6cf663c"></a>

# Org


<a id="orga172a20"></a>

## Unbind

C-, is reserved for switching buffers
C-c C-r is reserved for swiper

    (eval-after-load "org" '(define-key org-mode-map (kbd "C-,") nil))
    (eval-after-load "org" '(define-key org-mode-map (kbd "C-c C-r") nil))


<a id="orgd02fde5"></a>

## org-todo-keywords

    (setq org-todo-keywords
          '((sequence "TODO(t!)" "BUY(b!)" "BOOK(k!)" "LOOKUP(l!)" "|" "DONE(d!)")))


<a id="org33408b8"></a>

## org-super-agenda

Set org-agenda files

    (use-package org-super-agenda
      :ensure t
      :init
      (setq org-super-agenda-groups
            '((:name "Deadline"
                     :deadline t
                     :order 1)
              (:name "Todo"
                     :todo ("TODO")
                     :order 2)
              (:name "Book"
                     :todo ("BOOK")
                     :order 3)
              (:name "Lookup"
                     :todo ("LOOKUP")
                     :order 4)
              (:name "Buy"
                     :todo ("BUY")
                     :order 5))
            )
      :config
      (org-super-agenda-mode)
      (setq org-agenda-files
          (apply 'append
                 (mapcar
                  (lambda (directory)
                    (directory-files-recursively
                     directory org-agenda-file-regexp)
                    )
                  '("~/.emacs.d/"
                  "~/Documents/Private/")
                  )
                 )
          )
      )


<a id="org59237e8"></a>

## org-bullets

    (use-package org-bullets
     :ensure t
     :init
     (setq org-bullets-bullet-list
           '("ァ" "ィ" "ゥ" "ェ" "ォ"))
     :config
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


<a id="org9564067"></a>

## tags

    (setq org-tag-alist '(("@TRAVEL" . ?t)
                          ("@WEBSITE" . ?w)
                          ("KEYBOARD" . ?k)
                          ("@OREGON2020")
                          ))


<a id="org038913c"></a>

## custom

    (setq org-log-done 'time)


<a id="org37b10e4"></a>

# Keybinds


<a id="org24bd541"></a>

## end/start of buffer

    (global-set-key (kbd "<end>") `end-of-buffer)
    (global-set-key (kbd "<home>") `beginning-of-buffer)


<a id="org4f413c1"></a>

## Switch buffers

    (global-set-key (kbd "C-,")
    '(lambda()
    (interactive)
    (select-window (previous-window))))
    
    (global-set-key (kbd "C-.")
    '(lambda()
    (interactive)
    (select-window (next-window))))

