
# Table of Contents

1.  [Package setup](#org511a764)
2.  [Benchmark](#orgd360c1d)
3.  [Basic](#org0e99945)
    1.  [Annotate fix](#org21b5b8f)
    2.  [Skip gui](#orgb6fa135)
    3.  [Enables show-paren-mode](#org3e7f744)
    4.  [Auto-save and backup files saved in TEMP](#org0e197c8)
    5.  [Change all prompts to y or n](#org89f997b)
    6.  [settings for new frames (that is new window in windows, C-x 5 2)](#org1b58a21)
4.  [Modes](#orga0f34ee)
    1.  [rjsx-mode](#org5769e3f)
    2.  [nxml-mode](#org42775d6)
    3.  [web-mode](#org1878daf)
    4.  [cc-mode](#orga29dc60)
5.  [Functions](#org080d27e)
    1.  [Override alt-backspace](#org7e9a0a1)
    2.  [Delete line (without kill-ring) (Ctrl-Shift-K)](#org8665169)
    3.  [isearch-query-replace-symbol-at-point (M-s %)](#org8a902da)
6.  [Look and feel](#orgea38e86)
    1.  [Theme](#orgf3919bf)
        1.  [chocolate-theme](#org99dc595)
        2.  [dracula-theme](#org5ddc41f)
    2.  [Layout](#orgeb6aef2)
        1.  [Add line at top of the buffer to show column length](#org7e05334)
        2.  [Fringe git-gutter settings](#orgb2d36bf)
        3.  [yascroll 'no scroll bar'](#org052f564)
        4.  [smart-mode-line](#org7dd744a)
        5.  [Trailing whitespaces](#org2dcaa09)
        6.  [Layout functions](#org8891054)
        7.  [display-time-mode](#orgfb46814)
    3.  [Input](#orgfcf2a86)
        1.  [Insert matching delimiters](#org765b5cf)
        2.  [Indentation](#orgf3aef8f)
        3.  [I-search](#orgbd047b1)
7.  [Packages](#orgb7f959b)
    1.  [Clang-format](#org68c819b)
    2.  [Magit](#orgefab23c)
    3.  [git-timemachine](#org5d8e6be)
    4.  [swiper](#orgd513f95)
    5.  [multiple-cursors](#orgafaee67)
    6.  [helm](#org359ed73)
    7.  [expand-region](#org72936a0)
    8.  [move-text](#org2084e64)
    9.  [which-key](#orgf1bd61d)
    10. [doom-modeline](#orgfc0babb)
    11. [minimap](#org5aca9a0)
8.  [EXWM](#org44208aa)
    1.  [Add wm](#org8a5732c)
    2.  [exwm-package](#orgf8a27af)
    3.  [dmenu](#org709b7fd)
    4.  [systemtray](#orgdfa8f8f)
    5.  [randr](#org3fc9ebb)
    6.  [enable XF86](#org529d5f7)
    7.  [keybinds](#org72f6009)
    8.  [volume](#orga18607a)
    9.  [default browser](#orgab53362)
9.  [Org](#org061b3e0)
    1.  [Unbind](#org53d04c5)
    2.  [org-todo-keywords](#org03ca21a)
    3.  [org-super-agenda](#org587bf92)
    4.  [org-bullets](#org9c20663)
    5.  [tags](#org2b6ee73)
    6.  [org-capture-templates](#org664a35b)
10. [Keybinds](#org4f00569)
    1.  [end/start of buffer (<home> | <end>)](#org47ce501)
    2.  [Switch buffers (C-, | C-.)](#org6d09b8d)
    3.  [org-capture (C-c c)](#org9618d81)
    4.  [org-agenda (C-c a)](#org412e2ed)



<a id="org511a764"></a>

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


<a id="orgd360c1d"></a>

# Benchmark

    (use-package benchmark-init
     :ensure t)


<a id="org0e99945"></a>

# Basic


<a id="org21b5b8f"></a>

## Annotate fix

    (eval-after-load "vc-git"
      '(defun vc-git-annotate-command (file buf &optional rev)
        (let ((name (file-relative-name file)))
          (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))
    )


<a id="orgb6fa135"></a>

## Skip gui

    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    
    (set-variable 'tooltip-mode nil)
    (blink-cursor-mode -1)
    (column-number-mode t)
    (setq inhibit-splash-screen t)


<a id="org3e7f744"></a>

## Enables show-paren-mode

    (setq show-paren-delay 0)
    (show-paren-mode 1)


<a id="org0e197c8"></a>

## Auto-save and backup files saved in TEMP

    (setq backup-directory-alist
          `((".*" . ,"~/.saves")))
    (setq custom-file "~/.emacs.d/auto-custom.el")


<a id="org89f997b"></a>

## Change all prompts to y or n

    (fset 'yes-or-no-p 'y-or-n-p)


<a id="org1b58a21"></a>

## settings for new frames (that is new window in windows, C-x 5 2)

    (setq default-frame-alist
          '((font . "Fira Mono")
            (vertical-scroll-bars . nil)
            (menu-bar-lines . 0)
            (left-fringe . 0)))


<a id="orga0f34ee"></a>

# Modes


<a id="org5769e3f"></a>

## rjsx-mode

    (use-package rjsx-mode
      :ensure t)
    
    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
    (add-to-list 'interpreter-mode-alist '("node" . rjsx-mode))


<a id="org42775d6"></a>

## nxml-mode

    (add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))


<a id="org1878daf"></a>

## web-mode

    (use-package web-mode
      :ensure t
      :mode ("\\.html\\'"))


<a id="orga29dc60"></a>

## cc-mode

Switch between header and implementation file

    (eval-after-load "cc-mode"
      '(define-key c++-mode-map (kbd "C-x m") 'ff-find-other-file))


<a id="org080d27e"></a>

# Functions


<a id="org7e9a0a1"></a>

## Override alt-backspace

    (defun miker/backward-delete-word (arg)
      (interactive "p")
      (delete-region (point) (progn (backward-word arg) (point))))
    (global-set-key (kbd "M-<backspace>") 'miker/backward-delete-word)


<a id="org8665169"></a>

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


<a id="org8a902da"></a>

## isearch-query-replace-symbol-at-point (M-s %)

    (defun miker/isearch-query-replace-symbol-at-point ()
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp)
    )
    (global-set-key (kbd "M-s %") 'miker/isearch-query-replace-symbol-at-point)


<a id="orgea38e86"></a>

# Look and feel


<a id="orgf3919bf"></a>

## Theme


<a id="org99dc595"></a>

### chocolate-theme

    ;;(set-background-color "wheat2")
    ;;(use-package chocolate-theme
    ;;  :ensure t
    ;;  :config
    ;;  (load-theme 'chocolate t))


<a id="org5ddc41f"></a>

### dracula-theme

    (use-package dracula-theme
      :ensure t
      :config
      (load-theme 'dracula t))
    
    (set-face-attribute 'region nil :background "#342c6b" :foreground nil)


<a id="orgeb6aef2"></a>

## Layout


<a id="org7e05334"></a>

### Add line at top of the buffer to show column length

    (setq-default header-line-format
                  (list " " (make-string 76 ?-) "|"))

    ;;Highlight line
    ;;(global-hl-line-mode 1)
    ;;(set-face-background hl-line-face "darkgrey")
    (set-cursor-color "#C8FF03")


<a id="orgb2d36bf"></a>

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


<a id="org052f564"></a>

### yascroll 'no scroll bar'

    ;; No scroll bar
    (set-face-background 'vertical-border "snow4")
    (set-face-foreground 'vertical-border (face-background 'vertical-border))
    
    (use-package yascroll
      :ensure t)
    (global-yascroll-bar-mode 1)


<a id="org7dd744a"></a>

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


<a id="org2dcaa09"></a>

### Trailing whitespaces

    (setq-default show-trailing-whitespace t)


<a id="org8891054"></a>

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


<a id="orgfb46814"></a>

### display-time-mode

    (setq display-time-24hr-format t)
    (setq display-time-format "(%H:%M %e/%m)")
    (display-time-mode 1)


<a id="orgfcf2a86"></a>

## Input


<a id="org765b5cf"></a>

### Insert matching delimiters

    (electric-pair-mode 1)
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


<a id="orgf3aef8f"></a>

### Indentation

    (setq-default indent-tabs-mode nil)
    (setq-default c-basic-offset 4)
    (setq-default js2-indent-level 4)
    (setq-default sgml-basic-offset 4)
    (setq-default cmake-tab-width 4)
    (setq-default nxml-child-indent 4 nxml-attribute-indent 4)


<a id="orgbd047b1"></a>

### I-search

    (setq search-whitespace-regexp ".*?")


<a id="orgb7f959b"></a>

# Packages


<a id="org68c819b"></a>

## Clang-format

    (use-package clang-format
      :ensure t
      :bind
      (("C-c f" . clang-format)))


<a id="orgefab23c"></a>

## Magit

    (use-package magit
      :ensure t
      :commands (magit))


<a id="org5d8e6be"></a>

## git-timemachine

    (use-package git-timemachine
      :ensure t
      :commands (git-timemachine))


<a id="orgd513f95"></a>

## swiper

    (use-package swiper
      :ensure t
      :bind
      ("C-c C-r" . swiper)
      )


<a id="orgafaee67"></a>

## multiple-cursors

    (use-package multiple-cursors
      :ensure t
      :bind
      ("C->" . mc/mark-next-like-this)
      ("C-<" . mc/mark-previous-like-this)
      ("C-c C-<" . 'mc/mark-all-like-this)
    )


<a id="org359ed73"></a>

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


<a id="org72936a0"></a>

## expand-region

    (use-package expand-region
      :ensure t
      :bind
      ("C-=" . er/expand-region)
      ("C-;" . er/expand-region))


<a id="org2084e64"></a>

## move-text

    (use-package move-text
      :ensure t
      :bind
      (("C-S-p" . move-text-up)
       ("C-S-n" . move-text-down)))


<a id="orgf1bd61d"></a>

## which-key

    (use-package which-key
      :ensure t)
    (which-key-mode)


<a id="orgfc0babb"></a>

## doom-modeline

    (use-package all-the-icons)
    (use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-init)
    )


<a id="org5aca9a0"></a>

## minimap

    (use-package minimap
      :ensure t
      :custom
      (minimap-window-location 'right)
      :config
      ;;dracula color
      (set-face-attribute 'minimap-active-region-background nil :background "#342c6b")
    )


<a id="org44208aa"></a>

# EXWM

tangle yes/no to enable/disable EXWM


<a id="org8a5732c"></a>

## Add wm

    cd /usr/share/xsessions
    touch emacs.desktop

Add to emacs.desktop:

    [Desktop Entry]
    Name=EXWM
    Comment=Emacs window manager
    Exec=emacs
    Type=Application


<a id="orgf8a27af"></a>

## exwm-package

    (use-package exwm
      :ensure t
      :config
      (require 'exwm-config)
      (exwm-config-default)
    )


<a id="org709b7fd"></a>

## dmenu

    (use-package dmenu
      :ensure t
      :bind
      ("s-SPC" . 'dmenu))


<a id="orgdfa8f8f"></a>

## systemtray

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)


<a id="org3fc9ebb"></a>

## randr

    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist
    '(0 "DVI-D-0" 1 "DP-2"))
    (exwm-randr-enable)

Added in *home/<user>*.profile :
xrandr &#x2013;output DP-2 &#x2013;primary &#x2013;mode 2560x1440 &#x2013;rate 143.96
xrandr &#x2013;output DVI-D-0 &#x2013;mode 1920x1080 &#x2013;rotate right &#x2013;left-of DP-2

Run xrandr in term to see current settings


<a id="org529d5f7"></a>

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


<a id="org72f6009"></a>

## keybinds

    (global-set-key (kbd "s-k") 'exwm-workspace-delete)
    (global-set-key (kbd "s-w") 'exwm-workspace-swap)
    
    (global-set-key (kbd "<XF86ScreenSaver>") 'miker/launch-lock-screen)
    (global-set-key (kbd "<XF86PowerOff>") 'miker/launch-shutdown)


<a id="orga18607a"></a>

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


<a id="orgab53362"></a>

## default browser

    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "firefox")


<a id="org061b3e0"></a>

# Org


<a id="org53d04c5"></a>

## Unbind

C-, is reserved for switching buffers
C-c C-r is reserved for swiper

    (eval-after-load "org" '(define-key org-mode-map (kbd "C-,") nil))
    (eval-after-load "org" '(define-key org-mode-map (kbd "C-c C-r") nil))


<a id="org03ca21a"></a>

## org-todo-keywords

    (setq org-todo-keywords
          '((sequence "TODO(t!)" "BUY(b!)" "BOOK(k!)" "LOOKUP(l!)" "|" "DONE(d!)")))


<a id="org587bf92"></a>

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
                    ) '(
                  "~/Dropbox/org-mode/"
                  )))
          )
      )


<a id="org9c20663"></a>

## org-bullets

    (use-package org-bullets
     :ensure t
     :init
     (setq org-bullets-bullet-list
           '("ァ" "ィ" "ゥ" "ェ" "ォ"))
     :config
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


<a id="org2b6ee73"></a>

## tags

    (setq org-tag-alist '(("@TRAVEL" . ?t)
                          ("@WEBSITE" . ?w)
                          ("KEYBOARD" . ?k)
                          ("@OREGON2020")
                          ("@SCOTLAND2020")
                          ))


<a id="org664a35b"></a>

## org-capture-templates

    (setq org-capture-templates '(
                                  ("t" "Todo")
                                  ("tt" "Todo" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/todo.org" "Todo")
                                   "* TODO %?\nAdded: %U")
    
                                  ("tp" "Todo - Purchase" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/todo.org" "Todo")
                                   "* BUY %?\nAdded: %U")
    
                                  ("tr" "Todo - Travel" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/Trips/travel.org" "Travel")
                                   "* BOOK %?\nAdded: %U")
    
                                  ("k" "Keyboard")
                                  ("kt" "Keyboard - Todo" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/Projects/keyboard.org" "Keyboard")
                                   "* TODO %?\nAdded: %U")
    
                                  ("k6" "Keyboard - 65%" entry
                                   (file+olp
                                   "~/Dropbox/org-mode/Projects/keyboard.org" "Keyboard" "65%")
                                   "* TODO %?\nAdded: %U")
    
                                  ("r" "Travel")
                                  ("rr" "Travel - Travel" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/Trips/travel.org" "Travel")
                                   "* BOOK %?\nAdded: %U")
    
                                  ("r" "Travel")
                                  ("ro" "Travel - Oregon2020" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/Trips/usa-oregon-2020.org" "Oregon2020")
                                   "* TODO %?\nAdded: %U")
    
                                  ("r" "Travel")
                                  ("rs" "Travel - Scotland2020" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/Trips/scotland-2020.org" "Scotland2020")
                                   "* TODO %?\nAdded: %U")
                                 ))


<a id="org4f00569"></a>

# Keybinds


<a id="org47ce501"></a>

## end/start of buffer (<home> | <end>)

    (global-set-key (kbd "<home>") `beginning-of-buffer)
    (global-set-key (kbd "<end>") `end-of-buffer)


<a id="org6d09b8d"></a>

## Switch buffers (C-, | C-.)

    (global-set-key (kbd "C-,")
                    '(lambda()
                    (interactive)
                    (select-window (previous-window))
                    ))
    
    (global-set-key (kbd "C-.")
                    '(lambda()
                    (interactive)
                    (select-window (next-window))
    ))


<a id="org9618d81"></a>

## org-capture (C-c c)

    (global-set-key (kbd "C-c c") `org-capture)


<a id="org412e2ed"></a>

## org-agenda (C-c a)

    (global-set-key (kbd "C-c a") `org-agenda)

