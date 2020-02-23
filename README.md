
# Table of Contents

1.  [Package setup](#orgfaab949)
2.  [Benchmark](#orgf5ff077)
3.  [Basic](#orgd30da38)
    1.  [Annotate fix](#orgf7c3682)
    2.  [Skip gui](#org9259320)
    3.  [Enables show-paren-mode](#org39112c5)
    4.  [Auto-save and backup files saved in TEMP](#org18fc342)
    5.  [Change all prompts to y or n](#org9111806)
    6.  [settings for new frames (that is new window in windows, C-x 5 2)](#org7a2af01)
4.  [Modes](#orgc0937de)
    1.  [rjsx-mode](#org13851ef)
    2.  [nxml-mode](#org0cf15d3)
    3.  [web-mode](#orgda7d641)
    4.  [cc-mode](#orgfb89520)
5.  [Functions](#orgd595f53)
    1.  [Override alt-backspace](#org2106205)
    2.  [Delete line (without kill-ring) (Ctrl-Shift-K)](#org766f4b8)
    3.  [isearch-query-replace-symbol-at-point (M-s %)](#orgd35f7d7)
6.  [Look and feel](#orgd36abd0)
    1.  [Theme](#org9fe5cf9)
        1.  [chocolate-theme](#orgf4e7494)
        2.  [dracula-theme](#org70d7788)
    2.  [Layout](#org4f92866)
        1.  [Add line at top of the buffer to show column length](#org364b64e)
        2.  [Fringe git-gutter settings](#orgbb036ed)
        3.  [yascroll 'no scroll bar'](#orgd1db82d)
        4.  [smart-mode-line](#orgf8980d5)
        5.  [Trailing whitespaces](#orgf0a14fc)
        6.  [Layout functions](#orgc8667fa)
        7.  [display-time-mode](#orgc7f949b)
    3.  [Input](#org548ca1e)
        1.  [Insert matching delimiters](#org4daeff5)
        2.  [Indentation](#orgc8b8244)
        3.  [I-search](#orgd34cd2b)
7.  [Packages](#org41f17bf)
    1.  [Clang-format](#org4ae4862)
    2.  [Magit](#orgfbd0d44)
    3.  [git-timemachine](#org8f1018e)
    4.  [swiper](#org0d8dabf)
    5.  [multiple-cursors](#orgfbc2ac1)
    6.  [helm](#org2a19e6d)
    7.  [expand-region](#org967ed94)
    8.  [move-text](#org06a6af4)
    9.  [which-key](#orgd114ba6)
    10. [doom-modeline](#orgb798bab)
    11. [minimap](#org47a1e12)
8.  [EXWM](#org7a232ca)
    1.  [Add wm](#orgea4a0ce)
    2.  [exwm-package](#orga276ed2)
    3.  [dmenu](#org293d2c5)
    4.  [systemtray](#orga5ec072)
    5.  [randr](#orgdad105b)
    6.  [enable XF86](#org5af2c71)
    7.  [keybinds](#org6412139)
    8.  [volume](#org20f6337)
    9.  [default browser](#org8f5511b)
9.  [Org](#orgc73e248)
    1.  [Unbind](#org85e1be1)
    2.  [org-todo-keywords](#orgcfd0a44)
    3.  [org-super-agenda](#org150ba7c)
    4.  [org-bullets](#org77df385)
    5.  [tags](#orge85b8f8)
    6.  [org-capture-templates](#orgc5ad0a7)
10. [Keybinds](#org7062268)
    1.  [end/start of buffer (<home> | <end>)](#org685a149)
    2.  [Switch buffers (C-, | C-.)](#orgcaf4579)
    3.  [org-capture (C-c c)](#orgffdbc67)
    4.  [org-agenda (C-c a)](#orgf55e720)



<a id="orgfaab949"></a>

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


<a id="orgf5ff077"></a>

# Benchmark

    (use-package benchmark-init
     :ensure t)


<a id="orgd30da38"></a>

# Basic


<a id="orgf7c3682"></a>

## Annotate fix

    (eval-after-load "vc-git"
      '(defun vc-git-annotate-command (file buf &optional rev)
        (let ((name (file-relative-name file)))
          (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))
    )


<a id="org9259320"></a>

## Skip gui

    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    
    (set-variable 'tooltip-mode nil)
    (blink-cursor-mode -1)
    (column-number-mode t)
    (setq inhibit-splash-screen t)


<a id="org39112c5"></a>

## Enables show-paren-mode

    (setq show-paren-delay 0)
    (show-paren-mode 1)


<a id="org18fc342"></a>

## Auto-save and backup files saved in TEMP

    (setq backup-directory-alist
          `((".*" . ,"~/.saves")))
    (setq custom-file "~/.emacs.d/auto-custom.el")


<a id="org9111806"></a>

## Change all prompts to y or n

    (fset 'yes-or-no-p 'y-or-n-p)


<a id="org7a2af01"></a>

## settings for new frames (that is new window in windows, C-x 5 2)

    (setq default-frame-alist
          '((font . "Fira Mono")
            (vertical-scroll-bars . nil)
            (menu-bar-lines . 0)
            (left-fringe . 0)))


<a id="orgc0937de"></a>

# Modes


<a id="org13851ef"></a>

## rjsx-mode

    (use-package rjsx-mode
      :ensure t)
    
    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
    (add-to-list 'interpreter-mode-alist '("node" . rjsx-mode))


<a id="org0cf15d3"></a>

## nxml-mode

    (add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))


<a id="orgda7d641"></a>

## web-mode

    (use-package web-mode
      :ensure t
      :mode ("\\.html\\'"))


<a id="orgfb89520"></a>

## cc-mode

Switch between header and implementation file

    (eval-after-load "cc-mode"
      '(define-key c++-mode-map (kbd "C-x m") 'ff-find-other-file))


<a id="orgd595f53"></a>

# Functions


<a id="org2106205"></a>

## Override alt-backspace

    (defun miker/backward-delete-word (arg)
      (interactive "p")
      (delete-region (point) (progn (backward-word arg) (point))))
    (global-set-key (kbd "M-<backspace>") 'miker/backward-delete-word)


<a id="org766f4b8"></a>

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


<a id="orgd35f7d7"></a>

## isearch-query-replace-symbol-at-point (M-s %)

    (defun miker/isearch-query-replace-symbol-at-point ()
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp)
    )
    (global-set-key (kbd "M-s %") 'miker/isearch-query-replace-symbol-at-point)


<a id="orgd36abd0"></a>

# Look and feel


<a id="org9fe5cf9"></a>

## Theme


<a id="orgf4e7494"></a>

### chocolate-theme

    ;;(set-background-color "wheat2")
    ;;(use-package chocolate-theme
    ;;  :ensure t
    ;;  :config
    ;;  (load-theme 'chocolate t))


<a id="org70d7788"></a>

### dracula-theme

    (use-package dracula-theme
      :ensure t
      :config
      (load-theme 'dracula t))
    
    (set-face-attribute 'region nil :background "#342c6b" :foreground nil)


<a id="org4f92866"></a>

## Layout


<a id="org364b64e"></a>

### Add line at top of the buffer to show column length

    (setq-default header-line-format
                  (list " " (make-string 76 ?-) "|"))

    ;;Highlight line
    ;;(global-hl-line-mode 1)
    ;;(set-face-background hl-line-face "darkgrey")
    (set-cursor-color "#C8FF03")


<a id="orgbb036ed"></a>

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


<a id="orgd1db82d"></a>

### yascroll 'no scroll bar'

    ;; No scroll bar
    (set-face-background 'vertical-border "snow4")
    (set-face-foreground 'vertical-border (face-background 'vertical-border))
    
    (use-package yascroll
      :ensure t)
    (global-yascroll-bar-mode 1)


<a id="orgf8980d5"></a>

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


<a id="orgf0a14fc"></a>

### Trailing whitespaces

    (setq-default show-trailing-whitespace t)


<a id="orgc8667fa"></a>

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


<a id="orgc7f949b"></a>

### display-time-mode

    (setq display-time-24hr-format t)
    (setq display-time-format "(%H:%M %e/%m)")
    (display-time-mode 1)


<a id="org548ca1e"></a>

## Input


<a id="org4daeff5"></a>

### Insert matching delimiters

    (electric-pair-mode 1)
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


<a id="orgc8b8244"></a>

### Indentation

    (setq-default indent-tabs-mode nil)
    (setq-default c-basic-offset 4)
    (setq-default js2-indent-level 4)
    (setq-default sgml-basic-offset 4)
    (setq-default cmake-tab-width 4)
    (setq-default nxml-child-indent 4 nxml-attribute-indent 4)


<a id="orgd34cd2b"></a>

### I-search

    (setq search-whitespace-regexp ".*?")


<a id="org41f17bf"></a>

# Packages


<a id="org4ae4862"></a>

## Clang-format

    (use-package clang-format
      :ensure t
      :bind
      (("C-c f" . clang-format)))


<a id="orgfbd0d44"></a>

## Magit

    (use-package magit
      :ensure t
      :after (magit))


<a id="org8f1018e"></a>

## git-timemachine

    (use-package git-timemachine
      :ensure t
      :after (git-timemachine))


<a id="org0d8dabf"></a>

## swiper

    (use-package swiper
      :ensure t
      :bind
      ("C-c C-r" . swiper)
      )


<a id="orgfbc2ac1"></a>

## multiple-cursors

    (use-package multiple-cursors
      :ensure t
      :bind
      ("C->" . mc/mark-next-like-this)
      ("C-<" . mc/mark-previous-like-this)
      ("C-c C-<" . 'mc/mark-all-like-this)
    )


<a id="org2a19e6d"></a>

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


<a id="org967ed94"></a>

## expand-region

    (use-package expand-region
      :ensure t
      :bind
      ("C-=" . er/expand-region)
      ("C-;" . er/expand-region))


<a id="org06a6af4"></a>

## move-text

    (use-package move-text
      :ensure t
      :bind
      (("C-S-p" . move-text-up)
       ("C-S-n" . move-text-down)))


<a id="orgd114ba6"></a>

## which-key

    (use-package which-key
      :ensure t)
    (which-key-mode)


<a id="orgb798bab"></a>

## doom-modeline

    (use-package all-the-icons)
    (use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-init)
    )


<a id="org47a1e12"></a>

## minimap

    (use-package minimap
      :ensure t
      :custom
      (minimap-window-location 'right)
      :config
      ;;dracula color
      (set-face-attribute 'minimap-active-region-background nil :background "#342c6b")
    )


<a id="org7a232ca"></a>

# EXWM

tangle yes/no to enable/disable EXWM


<a id="orgea4a0ce"></a>

## Add wm

    cd /usr/share/xsessions
    touch emacs.desktop

Add to emacs.desktop:

    [Desktop Entry]
    Name=EXWM
    Comment=Emacs window manager
    Exec=emacs
    Type=Application


<a id="orga276ed2"></a>

## exwm-package

    (use-package exwm
      :ensure t
      :config
      (require 'exwm-config)
      (exwm-config-default)
    )


<a id="org293d2c5"></a>

## dmenu

    (use-package dmenu
      :ensure t
      :bind
      ("s-SPC" . 'dmenu))


<a id="orga5ec072"></a>

## systemtray

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)


<a id="orgdad105b"></a>

## randr

    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist
    '(0 "DVI-D-0" 1 "DP-2"))
    (exwm-randr-enable)

Added in *home/<user>*.profile :
xrandr &#x2013;output DP-2 &#x2013;primary &#x2013;mode 2560x1440 &#x2013;rate 143.96
xrandr &#x2013;output DVI-D-0 &#x2013;mode 1920x1080 &#x2013;rotate right &#x2013;left-of DP-2

Run xrandr in term to see current settings


<a id="org5af2c71"></a>

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


<a id="org6412139"></a>

## keybinds

    (global-set-key (kbd "s-k") 'exwm-workspace-delete)
    (global-set-key (kbd "s-w") 'exwm-workspace-swap)
    
    (global-set-key (kbd "<XF86ScreenSaver>") 'miker/launch-lock-screen)
    (global-set-key (kbd "<XF86PowerOff>") 'miker/launch-shutdown)


<a id="org20f6337"></a>

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


<a id="org8f5511b"></a>

## default browser

    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "firefox")


<a id="orgc73e248"></a>

# Org


<a id="org85e1be1"></a>

## Unbind

C-, is reserved for switching buffers
C-c C-r is reserved for swiper

    (eval-after-load "org" '(define-key org-mode-map (kbd "C-,") nil))
    (eval-after-load "org" '(define-key org-mode-map (kbd "C-c C-r") nil))


<a id="orgcfd0a44"></a>

## org-todo-keywords

    (setq org-todo-keywords
          '((sequence "TODO(t!)" "BUY(b!)" "BOOK(k!)" "LOOKUP(l!)" "|" "DONE(d!)")))


<a id="org150ba7c"></a>

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


<a id="org77df385"></a>

## org-bullets

    (use-package org-bullets
     :ensure t
     :init
     (setq org-bullets-bullet-list
           '("ァ" "ィ" "ゥ" "ェ" "ォ"))
     :config
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


<a id="orge85b8f8"></a>

## tags

    (setq org-tag-alist '(("@TRAVEL" . ?t)
                          ("@WEBSITE" . ?w)
                          ("KEYBOARD" . ?k)
                          ("@OREGON2020")
                          ("@SCOTLAND2020")
                          ))


<a id="orgc5ad0a7"></a>

## org-capture-templates

    (setq org-capture-templates '(
                                  ("t" "Todo")
                                  ("tt" "Todo" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/todo.org" "Todo")
                                   "* TODO %?\nAdded: %U")
    
                                   ("tw" "Todo - Website" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/todo.org" "Website")
                                   "* TODO %?\nAdded: %U")
    
                                  ("tr" "Todo - Travel" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/todo.org" "Travel")
                                   "* BOOK %?\nAdded: %U")
    
                                  ("tp" "Todo - Purchase" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/todo.org" "Purchase")
                                   "* BUY %?\nAdded: %U")
    
                                  ("tk" "Todo - Keyboard")
                                  ("tkk" "Todo - Keyboard" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/todo.org" "Keyboard")
                                   "* TODO %?\nAdded: %U")
    
                                  ("tk6" "Todo - Keyboard - 65%" entry
                                   (file+olp
                                   "~/Dropbox/org-mode/todo.org" "Todo" "Keyboard" "65%")
                                   "* TODO %?\nAdded: %U")
    
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


<a id="org7062268"></a>

# Keybinds


<a id="org685a149"></a>

## end/start of buffer (<home> | <end>)

    (global-set-key (kbd "<home>") `beginning-of-buffer)
    (global-set-key (kbd "<end>") `end-of-buffer)


<a id="orgcaf4579"></a>

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


<a id="orgffdbc67"></a>

## org-capture (C-c c)

    (global-set-key (kbd "C-c c") `org-capture)


<a id="orgf55e720"></a>

## org-agenda (C-c a)

    (global-set-key (kbd "C-c a") `org-agenda)

