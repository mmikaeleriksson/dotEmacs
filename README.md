
# Table of Contents

1.  [Package setup](#org1f56876)
2.  [Benchmark](#org752cd54)
3.  [Basic](#orgda619e1)
    1.  [Annotate fix](#orge2b8a38)
    2.  [Skip gui](#orgc0f8db9)
    3.  [Enables show-paren-mode](#org1781295)
    4.  [Auto-save and backup files saved in TEMP](#orgcf350c7)
    5.  [Change all prompts to y or n](#org4db1777)
    6.  [settings for new frames (that is new window in windows, C-x 5 2)](#org6ff1020)
4.  [Modes](#orgc303ebe)
    1.  [rjsx-mode](#org8e50841)
    2.  [nxml-mode](#org986d297)
    3.  [web-mode](#orgc1057fa)
    4.  [cc-mode](#org47126b2)
5.  [Functions](#orgfb11c77)
    1.  [Override alt-backspace](#orgcd52701)
    2.  [Delete line (without kill-ring) (Ctrl-Shift-K)](#orgf44cafc)
    3.  [isearch-query-replace-symbol-at-point (M-s %)](#orgdb639cb)
6.  [Look and feel](#orgae60034)
    1.  [Theme](#org0376770)
        1.  [chocolate-theme](#org1e586de)
        2.  [dracula-theme](#orge1fb300)
    2.  [Layout](#org83d8576)
        1.  [Add line at top of the buffer to show column length](#orgc3c0b76)
        2.  [Fringe git-gutter settings](#org0331565)
        3.  [yascroll 'no scroll bar'](#org8b778e7)
        4.  [smart-mode-line](#orgc40d56e)
        5.  [Trailing whitespaces](#orgf7c4294)
        6.  [Layout functions](#org285817b)
        7.  [display-time-mode](#orgca137ec)
    3.  [Input](#org69d54e1)
        1.  [Insert matching delimiters](#org2c005bd)
        2.  [Indentation](#orgd2ef98e)
        3.  [I-search](#org0c8f903)
7.  [Packages](#org705eb8d)
    1.  [Clang-format](#orgb3b270e)
    2.  [Magit](#org3c9d5ca)
    3.  [git-timemachine](#org1e09779)
    4.  [swiper](#org867ce52)
    5.  [multiple-cursors](#orgdaad19d)
    6.  [helm](#orgf209255)
    7.  [expand-region](#org0fc67f1)
    8.  [move-text](#orgcaf1c5c)
    9.  [which-key](#org2ff4e7d)
    10. [doom-modeline](#org0b9c3b3)
    11. [minimap](#org1118841)
8.  [EXWM](#org105c546)
    1.  [Add wm](#orgaa49035)
    2.  [exwm-package](#org0952315)
    3.  [dmenu](#org1c16d22)
    4.  [systemtray](#orgca36385)
    5.  [randr](#org275f07b)
    6.  [enable XF86](#orgd872a37)
    7.  [keybinds](#orgc3a3313)
    8.  [volume](#org782d304)
    9.  [default browser](#org7bfd574)
9.  [Org](#org5a801e5)
    1.  [Unbind](#org739103a)
    2.  [org-todo-keywords](#org8ed1bed)
    3.  [org-super-agenda](#orgc38638a)
    4.  [org-bullets](#orgffd29ce)
    5.  [tags](#orgbad9535)
    6.  [org-capture-templates](#orgd21a891)
10. [Keybinds](#org7e81a03)
    1.  [end/start of buffer (<home> | <end>)](#orgda1a711)
    2.  [Switch buffers (C-, | C-.)](#orge206182)
    3.  [org-capture (C-c c)](#orgd1e5d6d)
    4.  [org-agenda (C-c a)](#org2e2bad0)



<a id="org1f56876"></a>

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


<a id="org752cd54"></a>

# Benchmark

    (use-package benchmark-init
     :ensure t)


<a id="orgda619e1"></a>

# Basic


<a id="orge2b8a38"></a>

## Annotate fix

    (eval-after-load "vc-git"
      '(defun vc-git-annotate-command (file buf &optional rev)
        (let ((name (file-relative-name file)))
          (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))
    )


<a id="orgc0f8db9"></a>

## Skip gui

    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    
    (set-variable 'tooltip-mode nil)
    (blink-cursor-mode -1)
    (column-number-mode t)
    (setq inhibit-splash-screen t)


<a id="org1781295"></a>

## Enables show-paren-mode

    (setq show-paren-delay 0)
    (show-paren-mode 1)


<a id="orgcf350c7"></a>

## Auto-save and backup files saved in TEMP

    (setq backup-directory-alist
          `((".*" . ,"~/.saves")))
    (setq custom-file "~/.emacs.d/auto-custom.el")


<a id="org4db1777"></a>

## Change all prompts to y or n

    (fset 'yes-or-no-p 'y-or-n-p)


<a id="org6ff1020"></a>

## settings for new frames (that is new window in windows, C-x 5 2)

    (setq default-frame-alist
          '((font . "Fira Mono")
            (vertical-scroll-bars . nil)
            (menu-bar-lines . 0)
            (left-fringe . 0)))


<a id="orgc303ebe"></a>

# Modes


<a id="org8e50841"></a>

## rjsx-mode

    (use-package rjsx-mode
      :ensure t)
    
    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
    (add-to-list 'interpreter-mode-alist '("node" . rjsx-mode))


<a id="org986d297"></a>

## nxml-mode

    (add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))


<a id="orgc1057fa"></a>

## web-mode

    (use-package web-mode
      :ensure t
      :mode ("\\.html\\'"))


<a id="org47126b2"></a>

## cc-mode

Switch between header and implementation file

    (eval-after-load "cc-mode"
      '(define-key c++-mode-map (kbd "C-x m") 'ff-find-other-file))


<a id="orgfb11c77"></a>

# Functions


<a id="orgcd52701"></a>

## Override alt-backspace

    (defun miker/backward-delete-word (arg)
      (interactive "p")
      (delete-region (point) (progn (backward-word arg) (point))))
    (global-set-key (kbd "M-<backspace>") 'miker/backward-delete-word)


<a id="orgf44cafc"></a>

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


<a id="orgdb639cb"></a>

## isearch-query-replace-symbol-at-point (M-s %)

    (defun miker/isearch-query-replace-symbol-at-point ()
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp)
    )
    (global-set-key (kbd "M-s %") 'miker/isearch-query-replace-symbol-at-point)


<a id="orgae60034"></a>

# Look and feel


<a id="org0376770"></a>

## Theme


<a id="org1e586de"></a>

### chocolate-theme

    ;;(set-background-color "wheat2")
    ;;(use-package chocolate-theme
    ;;  :ensure t
    ;;  :config
    ;;  (load-theme 'chocolate t))


<a id="orge1fb300"></a>

### dracula-theme

    (use-package dracula-theme
      :ensure t
      :config
      (load-theme 'dracula t))
    
    (set-face-attribute 'region nil :background "#342c6b" :foreground nil)


<a id="org83d8576"></a>

## Layout


<a id="orgc3c0b76"></a>

### Add line at top of the buffer to show column length

    (setq-default header-line-format
                  (list " " (make-string 76 ?-) "|"))

    ;;Highlight line
    ;;(global-hl-line-mode 1)
    ;;(set-face-background hl-line-face "darkgrey")
    (set-cursor-color "#C8FF03")


<a id="org0331565"></a>

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


<a id="org8b778e7"></a>

### yascroll 'no scroll bar'

    ;; No scroll bar
    (set-face-background 'vertical-border "snow4")
    (set-face-foreground 'vertical-border (face-background 'vertical-border))
    
    (use-package yascroll
      :ensure t)
    (global-yascroll-bar-mode 1)


<a id="orgc40d56e"></a>

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


<a id="orgf7c4294"></a>

### Trailing whitespaces

    (setq-default show-trailing-whitespace t)


<a id="org285817b"></a>

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


<a id="orgca137ec"></a>

### display-time-mode

    (setq display-time-24hr-format t)
    (setq display-time-format "(%H:%M %e/%m)")
    (display-time-mode 1)


<a id="org69d54e1"></a>

## Input


<a id="org2c005bd"></a>

### Insert matching delimiters

    (electric-pair-mode 1)
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


<a id="orgd2ef98e"></a>

### Indentation

    (setq-default indent-tabs-mode nil)
    (setq-default c-basic-offset 4)
    (setq-default js2-indent-level 4)
    (setq-default sgml-basic-offset 4)
    (setq-default cmake-tab-width 4)
    (setq-default nxml-child-indent 4 nxml-attribute-indent 4)


<a id="org0c8f903"></a>

### I-search

    (setq search-whitespace-regexp ".*?")


<a id="org705eb8d"></a>

# Packages


<a id="orgb3b270e"></a>

## Clang-format

    (use-package clang-format
      :ensure t
      :bind
      (("C-c f" . clang-format)))


<a id="org3c9d5ca"></a>

## Magit

    (use-package magit
      :ensure t
      :commands (magit))


<a id="org1e09779"></a>

## git-timemachine

    (use-package git-timemachine
      :ensure t
      :commands (git-timemachine))


<a id="org867ce52"></a>

## swiper

    (use-package swiper
      :ensure t
      :bind
      ("C-c C-r" . swiper)
      )


<a id="orgdaad19d"></a>

## multiple-cursors

    (use-package multiple-cursors
      :ensure t
      :bind
      ("C->" . mc/mark-next-like-this)
      ("C-<" . mc/mark-previous-like-this)
      ("C-c C-<" . 'mc/mark-all-like-this)
    )


<a id="orgf209255"></a>

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


<a id="org0fc67f1"></a>

## expand-region

    (use-package expand-region
      :ensure t
      :bind
      ("C-=" . er/expand-region)
      ("C-;" . er/expand-region))


<a id="orgcaf1c5c"></a>

## move-text

    (use-package move-text
      :ensure t
      :bind
      (("C-S-p" . move-text-up)
       ("C-S-n" . move-text-down)))


<a id="org2ff4e7d"></a>

## which-key

    (use-package which-key
      :ensure t)
    (which-key-mode)


<a id="org0b9c3b3"></a>

## doom-modeline

    (use-package all-the-icons)
    (use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-init)
    )


<a id="org1118841"></a>

## minimap

    (use-package minimap
      :ensure t
      :custom
      (minimap-window-location 'right)
      :config
      ;;dracula color
      (set-face-attribute 'minimap-active-region-background nil :background "#342c6b")
    )


<a id="org105c546"></a>

# EXWM

tangle yes/no to enable/disable EXWM


<a id="orgaa49035"></a>

## Add wm

    cd /usr/share/xsessions
    touch emacs.desktop

Add to emacs.desktop:

    [Desktop Entry]
    Name=EXWM
    Comment=Emacs window manager
    Exec=emacs
    Type=Application


<a id="org0952315"></a>

## exwm-package

    (use-package exwm
      :ensure t
      :config
      (require 'exwm-config)
      (exwm-config-default)
    )


<a id="org1c16d22"></a>

## dmenu

    (use-package dmenu
      :ensure t
      :bind
      ("s-SPC" . 'dmenu))


<a id="orgca36385"></a>

## systemtray

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)


<a id="org275f07b"></a>

## randr

    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist
    '(0 "DVI-D-0" 1 "DP-2"))
    (exwm-randr-enable)

Added in *home/<user>*.profile :
xrandr &#x2013;output DP-2 &#x2013;primary &#x2013;mode 2560x1440 &#x2013;rate 143.96
xrandr &#x2013;output DVI-D-0 &#x2013;mode 1920x1080 &#x2013;rotate right &#x2013;left-of DP-2

Run xrandr in term to see current settings


<a id="orgd872a37"></a>

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


<a id="orgc3a3313"></a>

## keybinds

    (global-set-key (kbd "s-k") 'exwm-workspace-delete)
    (global-set-key (kbd "s-w") 'exwm-workspace-swap)
    
    (global-set-key (kbd "<XF86ScreenSaver>") 'miker/launch-lock-screen)
    (global-set-key (kbd "<XF86PowerOff>") 'miker/launch-shutdown)


<a id="org782d304"></a>

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


<a id="org7bfd574"></a>

## default browser

    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "firefox")


<a id="org5a801e5"></a>

# Org


<a id="org739103a"></a>

## Unbind

C-, is reserved for switching buffers
C-c C-r is reserved for swiper

    (eval-after-load "org" '(define-key org-mode-map (kbd "C-,") nil))
    (eval-after-load "org" '(define-key org-mode-map (kbd "C-c C-r") nil))


<a id="org8ed1bed"></a>

## org-todo-keywords

    (setq org-todo-keywords
          '((sequence "TODO(t!)" "BUY(b!)" "BOOK(k!)" "LOOKUP(l!)" "|" "DONE(d!)")))


<a id="orgc38638a"></a>

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


<a id="orgffd29ce"></a>

## org-bullets

    (use-package org-bullets
     :ensure t
     :init
     (setq org-bullets-bullet-list
           '("ァ" "ィ" "ゥ" "ェ" "ォ"))
     :config
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


<a id="orgbad9535"></a>

## tags

    (setq org-tag-alist '(("@TRAVEL" . ?t)
                          ("@WEBSITE" . ?w)
                          ("KEYBOARD" . ?k)
                          ("@OREGON2020")
                          ("@SCOTLAND2020")
                          ))


<a id="orgd21a891"></a>

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


<a id="org7e81a03"></a>

# Keybinds


<a id="orgda1a711"></a>

## end/start of buffer (<home> | <end>)

    (global-set-key (kbd "<home>") `beginning-of-buffer)
    (global-set-key (kbd "<end>") `end-of-buffer)


<a id="orge206182"></a>

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


<a id="orgd1e5d6d"></a>

## org-capture (C-c c)

    (global-set-key (kbd "C-c c") `org-capture)


<a id="org2e2bad0"></a>

## org-agenda (C-c a)

    (global-set-key (kbd "C-c a") `org-agenda)

