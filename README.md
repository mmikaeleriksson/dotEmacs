
# Table of Contents

1.  [Package setup](#orgf21f589)
2.  [Benchmark](#orgcf22f49)
3.  [Basic](#orga1ef7a7)
    1.  [Annotate fix](#org1c8d072)
    2.  [Skip gui](#orgbfa3de9)
    3.  [Enables show-paren-mode](#org8b5b9d2)
    4.  [Auto-save and backup files saved in TEMP](#orga848e28)
    5.  [Change all prompts to y or n](#orgf552a57)
    6.  [settings for new frames (that is new window in windows, C-x 5 2)](#org195c821)
4.  [Modes](#orge028e36)
    1.  [rjsx-mode](#orgceef3b6)
    2.  [nxml-mode](#org6a0de6f)
    3.  [web-mode](#org6722180)
    4.  [cc-mode](#orge7d0982)
5.  [Functions](#org746a8e0)
    1.  [Override alt-backspace](#org3259034)
    2.  [Delete line (without kill-ring) (Ctrl-Shift-K)](#org26e516f)
    3.  [isearch-query-replace-symbol-at-point (M-s %)](#org6a6c305)
6.  [Look and feel](#org6793112)
    1.  [Theme](#org6573781)
        1.  [chocolate-theme](#orgba090ec)
        2.  [dracula-theme](#org996f134)
    2.  [Layout](#org4f86134)
        1.  [Add line at top of the buffer to show column length](#orgbfb6e5e)
        2.  [Fringe git-gutter settings](#org84f6fcc)
        3.  [yascroll 'no scroll bar'](#org20eb4ba)
        4.  [smart-mode-line](#org228192b)
        5.  [Trailing whitespaces](#org586555e)
        6.  [Layout functions](#org402dd31)
        7.  [display-time-mode](#orga19c163)
    3.  [Input](#orgdb5206e)
        1.  [Insert matching delimiters](#org566e3d9)
        2.  [Indentation](#orgff607de)
        3.  [I-search](#orgf353594)
7.  [Packages](#org60a1f0c)
    1.  [Clang-format](#org52e2e67)
    2.  [Magit](#org7827b1c)
    3.  [git-timemachine](#org9d3fb06)
    4.  [swiper](#orgd4b66b3)
    5.  [multiple-cursors](#org5996f70)
    6.  [helm](#org36b9251)
    7.  [expand-region](#org51ab16a)
    8.  [move-text](#org51ac945)
    9.  [which-key](#orge294a17)
    10. [doom-modeline](#org731a17b)
    11. [minimap](#orgf2bd37a)
    12. [mu4e (disabled)](#orgba41818)
8.  [EXWM](#orgcad809b)
    1.  [Add wm](#org48f18bf)
    2.  [exwm-package](#org3cc4751)
    3.  [dmenu](#orgac1227f)
    4.  [systemtray](#org8552dad)
    5.  [randr](#org34d720b)
    6.  [enable XF86](#orge8a6218)
    7.  [keybinds](#orge51996c)
    8.  [volume](#org974ebfe)
    9.  [default browser](#org2831ebc)
9.  [Org](#org54dc541)
    1.  [Unbind](#orga876bd9)
    2.  [org-todo-keywords](#org2af42fe)
    3.  [org-super-agenda](#org407df9f)
    4.  [org-bullets](#org15a9af7)
    5.  [tags](#orgecc7d28)
    6.  [org-capture-templates](#orge6f8e29)
    7.  [org-agenda](#org2d0f324)
10. [Keybinds](#orgd61573d)
    1.  [end/start of buffer (<home> | <end>)](#org1832e12)
    2.  [Switch buffers (C-, | C-.)](#orga7fd9f6)
    3.  [org-capture (C-c c)](#org42fa4ff)
    4.  [org-agenda (C-c a)](#org66bacb5)



<a id="orgf21f589"></a>

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


<a id="orgcf22f49"></a>

# Benchmark

    (use-package benchmark-init
     :ensure t)


<a id="orga1ef7a7"></a>

# Basic


<a id="org1c8d072"></a>

## Annotate fix

    (eval-after-load "vc-git"
      '(defun vc-git-annotate-command (file buf &optional rev)
        (let ((name (file-relative-name file)))
          (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))
    )


<a id="orgbfa3de9"></a>

## Skip gui

    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    
    (set-variable 'tooltip-mode nil)
    (blink-cursor-mode -1)
    (column-number-mode t)
    (setq inhibit-splash-screen t)


<a id="org8b5b9d2"></a>

## Enables show-paren-mode

    (setq show-paren-delay 0)
    (show-paren-mode 1)


<a id="orga848e28"></a>

## Auto-save and backup files saved in TEMP

    (setq backup-directory-alist
          `((".*" . ,"~/.saves")))
    (setq custom-file "~/.emacs.d/auto-custom.el")


<a id="orgf552a57"></a>

## Change all prompts to y or n

    (fset 'yes-or-no-p 'y-or-n-p)


<a id="org195c821"></a>

## settings for new frames (that is new window in windows, C-x 5 2)

    (setq default-frame-alist
          '((font . "Fira Mono")
            (vertical-scroll-bars . nil)
            (menu-bar-lines . 0)
            (left-fringe . 0)))


<a id="orge028e36"></a>

# Modes


<a id="orgceef3b6"></a>

## rjsx-mode

    (use-package rjsx-mode
      :ensure t)
    
    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
    (add-to-list 'interpreter-mode-alist '("node" . rjsx-mode))


<a id="org6a0de6f"></a>

## nxml-mode

    (add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))


<a id="org6722180"></a>

## web-mode

    (use-package web-mode
      :ensure t
      :mode ("\\.html\\'"))


<a id="orge7d0982"></a>

## cc-mode

Switch between header and implementation file

    (eval-after-load "cc-mode"
      '(define-key c++-mode-map (kbd "C-x m") 'ff-find-other-file))


<a id="org746a8e0"></a>

# Functions


<a id="org3259034"></a>

## Override alt-backspace

    (defun miker/backward-delete-word (arg)
      (interactive "p")
      (delete-region (point) (progn (backward-word arg) (point))))
    (global-set-key (kbd "M-<backspace>") 'miker/backward-delete-word)


<a id="org26e516f"></a>

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


<a id="org6a6c305"></a>

## isearch-query-replace-symbol-at-point (M-s %)

    (defun miker/isearch-query-replace-symbol-at-point ()
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp)
    )
    (global-set-key (kbd "M-s %") 'miker/isearch-query-replace-symbol-at-point)


<a id="org6793112"></a>

# Look and feel


<a id="org6573781"></a>

## Theme


<a id="orgba090ec"></a>

### chocolate-theme

    ;;(set-background-color "wheat2")
    ;;(use-package chocolate-theme
    ;;  :ensure t
    ;;  :config
    ;;  (load-theme 'chocolate t))


<a id="org996f134"></a>

### dracula-theme

    (use-package dracula-theme
      :ensure t
      :config
      (load-theme 'dracula t))
    
    (set-face-attribute 'region nil :background "#342c6b" :foreground nil)


<a id="org4f86134"></a>

## Layout


<a id="orgbfb6e5e"></a>

### Add line at top of the buffer to show column length

    (setq-default header-line-format
                  (list " " (make-string 76 ?-) "|"))

    ;;Highlight line
    ;;(global-hl-line-mode 1)
    ;;(set-face-background hl-line-face "darkgrey")
    (set-cursor-color "#C8FF03")


<a id="org84f6fcc"></a>

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


<a id="org20eb4ba"></a>

### yascroll 'no scroll bar'

    ;; No scroll bar
    (set-face-background 'vertical-border "snow4")
    (set-face-foreground 'vertical-border (face-background 'vertical-border))
    
    (use-package yascroll
      :ensure t)
    (global-yascroll-bar-mode 1)


<a id="org228192b"></a>

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


<a id="org586555e"></a>

### Trailing whitespaces

    (setq-default show-trailing-whitespace t)


<a id="org402dd31"></a>

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


<a id="orga19c163"></a>

### display-time-mode

    (setq display-time-24hr-format t)
    (setq display-time-format "(%H:%M %e/%m)")
    (display-time-mode 1)


<a id="orgdb5206e"></a>

## Input


<a id="org566e3d9"></a>

### Insert matching delimiters

    (electric-pair-mode 1)
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


<a id="orgff607de"></a>

### Indentation

    (setq-default indent-tabs-mode nil)
    (setq-default c-basic-offset 4)
    (setq-default js2-indent-level 4)
    (setq-default sgml-basic-offset 4)
    (setq-default cmake-tab-width 4)
    (setq-default nxml-child-indent 4 nxml-attribute-indent 4)


<a id="orgf353594"></a>

### I-search

    (setq search-whitespace-regexp ".*?")


<a id="org60a1f0c"></a>

# Packages


<a id="org52e2e67"></a>

## Clang-format

    (use-package clang-format
      :ensure t
      :bind
      (("C-c f" . clang-format)))


<a id="org7827b1c"></a>

## Magit

    (use-package magit
      :ensure t
      :commands (magit))


<a id="org9d3fb06"></a>

## git-timemachine

    (use-package git-timemachine
      :ensure t
      :commands (git-timemachine))


<a id="orgd4b66b3"></a>

## swiper

    (use-package swiper
      :ensure t
      :bind
      ("C-c C-r" . swiper)
      )


<a id="org5996f70"></a>

## multiple-cursors

    (use-package multiple-cursors
      :ensure t
      :bind
      ("C->" . mc/mark-next-like-this)
      ("C-<" . mc/mark-previous-like-this)
      ("C-c C-<" . 'mc/mark-all-like-this)
    )


<a id="org36b9251"></a>

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


<a id="org51ab16a"></a>

## expand-region

    (use-package expand-region
      :ensure t
      :bind
      ("C-=" . er/expand-region)
      ("C-;" . er/expand-region))


<a id="org51ac945"></a>

## move-text

    (use-package move-text
      :ensure t
      :bind
      (("C-S-p" . move-text-up)
       ("C-S-n" . move-text-down)))


<a id="orge294a17"></a>

## which-key

    (use-package which-key
      :ensure t)
    (which-key-mode)


<a id="org731a17b"></a>

## doom-modeline

    (use-package all-the-icons)
    (use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-init)
    )


<a id="orgf2bd37a"></a>

## minimap

    (use-package minimap
      :ensure t
      :custom
      (minimap-window-location 'right)
      :config
      ;;dracula color
      (set-face-attribute 'minimap-active-region-background nil :background "#342c6b")
    )


<a id="orgba41818"></a>

## mu4e (disabled)

    (use-package mu4e
      :ensure nil
      :custom
      (mu4e-attachment-dir "~/Downloads")
      (mu4e-maildir "~/Maildir")
      (mu4e-get-mail-command "mbsync -c ~/.mbsyncrc -a")
      (mu4e-view-show-images t)
      (mu4e-compose-in-new-frame t)
      (mu4e-sent-messages-behavior 'delete)
      (mu4e-change-filenames-when-moving t)
    )


<a id="orgcad809b"></a>

# EXWM

tangle yes/no to enable/disable EXWM


<a id="org48f18bf"></a>

## Add wm

    cd /usr/share/xsessions
    touch emacs.desktop

Add to emacs.desktop:

    [Desktop Entry]
    Name=EXWM
    Comment=Emacs window manager
    Exec=emacs
    Type=Application


<a id="org3cc4751"></a>

## exwm-package

    (use-package exwm
      :ensure t
      :config
      (require 'exwm-config)
      (exwm-config-default)
    )


<a id="orgac1227f"></a>

## dmenu

    (use-package dmenu
      :ensure t
      :bind
      ("s-SPC" . 'dmenu))


<a id="org8552dad"></a>

## systemtray

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)


<a id="org34d720b"></a>

## randr

    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist
    '(0 "DVI-D-0" 1 "DP-2"))
    (exwm-randr-enable)

Added in *home/<user>*.profile :
xrandr &#x2013;output DP-2 &#x2013;primary &#x2013;mode 2560x1440 &#x2013;rate 143.96
xrandr &#x2013;output DVI-D-0 &#x2013;mode 1920x1080 &#x2013;rotate right &#x2013;left-of DP-2

Run xrandr in term to see current settings


<a id="orge8a6218"></a>

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


<a id="orge51996c"></a>

## keybinds

    (global-set-key (kbd "s-k") 'exwm-workspace-delete)
    (global-set-key (kbd "s-w") 'exwm-workspace-swap)
    
    (global-set-key (kbd "<XF86ScreenSaver>") 'miker/launch-lock-screen)
    (global-set-key (kbd "<XF86PowerOff>") 'miker/launch-shutdown)


<a id="org974ebfe"></a>

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


<a id="org2831ebc"></a>

## default browser

    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "firefox")


<a id="org54dc541"></a>

# Org


<a id="orga876bd9"></a>

## Unbind

C-, is reserved for switching buffers
C-c C-r is reserved for swiper

    (eval-after-load "org" '(define-key org-mode-map (kbd "C-,") nil))
    (eval-after-load "org" '(define-key org-mode-map (kbd "C-c C-r") nil))


<a id="org2af42fe"></a>

## org-todo-keywords

    (setq org-todo-keywords
          '((sequence "TODO(t!)" "BUY(b!)" "BOOK(k!)" "LOOKUP(l!)" "|" "DONE(d!)")))


<a id="org407df9f"></a>

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


<a id="org15a9af7"></a>

## org-bullets

    (use-package org-bullets
     :ensure t
     :init
     (setq org-bullets-bullet-list
           '("ァ" "ィ" "ゥ" "ェ" "ォ"))
     :config
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


<a id="orgecc7d28"></a>

## tags

    (setq org-tag-alist '(("@TRAVEL" . ?t)
                          ("@WEBSITE" . ?w)
                          ("KEYBOARD" . ?k)
                          ("@OREGON2020")
                          ("@SCOTLAND2020")
                          ("@LINUX" . ?l)
                          ("@EMACS" . ?e)
                          ))


<a id="orge6f8e29"></a>

## org-capture-templates

    (setq org-capture-templates '(
                                  ("t" "Todo")
                                  ("tt" "Todo" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/todo.org" "Todo")
                                   "* TODO %?\nAdded: %U")
    
                                  ("tg" "Todo - Groceries" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/groceries.org" "Groceries")
                                   "* BUY %?\nAdded: %U")
    
                                  ("tp" "Todo - Purchase" entry
                                   (file+headline
                                   "~/Dropbox/org-mode/shopping.org" "Shopping")
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


<a id="org2d0f324"></a>

## org-agenda

    (setq org-agenda-window-setup 'current-window)


<a id="orgd61573d"></a>

# Keybinds


<a id="org1832e12"></a>

## end/start of buffer (<home> | <end>)

    (global-set-key (kbd "<home>") `beginning-of-buffer)
    (global-set-key (kbd "<end>") `end-of-buffer)


<a id="orga7fd9f6"></a>

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


<a id="org42fa4ff"></a>

## org-capture (C-c c)

    (global-set-key (kbd "C-c c") `org-capture)


<a id="org66bacb5"></a>

## org-agenda (C-c a)

    (global-set-key (kbd "C-c a") `org-agenda)

