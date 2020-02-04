
# Table of Contents

1.  [Package setup](#org6a9155f)
2.  [Benchmark](#org5a6c80b)
3.  [Basic](#orge47010d)
    1.  [Annotate fix](#org763fd1b)
    2.  [Skip gui](#org99e4c6a)
    3.  [Enables show-paren-mode](#org7099dbd)
    4.  [Auto-save and backup files saved in TEMP](#orgfc9e5d6)
    5.  [Change all prompts to y or n](#orga8c969e)
    6.  [settings for new frames (that is new window in windows, C-x 5 2)](#org22aecf2)
4.  [Modes](#org8d89526)
    1.  [js2-mode](#orgb62572b)
    2.  [nxml-mode](#org5e350c5)
    3.  [web-mode](#org61ba1eb)
    4.  [cc-mode](#org8c59562)
5.  [Functions](#org73f12c5)
    1.  [Override alt-backspace](#org3be4add)
    2.  [Delete line (without kill-ring) (Ctrl-Shift-K)](#orgeefa6ce)
    3.  [isearch-query-replace-symbol-at-point (M-s %)](#org816bc6a)
6.  [Look and feel](#org1a4e8b8)
    1.  [Theme](#org065b3e0)
        1.  [chocolate-theme](#orgb65dd6b)
        2.  [dracula-theme](#org1e1f170)
    2.  [Layout](#org6468a35)
        1.  [Add line at top of the buffer to show column length](#org61eb36c)
        2.  [Fringe git-gutter settings](#orgf177076)
        3.  [yascroll 'no scroll bar'](#org34e07ad)
        4.  [smart-mode-line](#org948b17f)
        5.  [Trailing whitespaces](#org5902300)
        6.  [Layout functions](#org4532660)
        7.  [display-time-mode](#org6faac3a)
    3.  [Input](#org090af9a)
        1.  [Insert matching delimiters](#orgb8ad939)
        2.  [Indentation](#org4639cc3)
        3.  [I-search](#org5660ad7)
7.  [Packages](#org2305f5f)
    1.  [Clang-format](#org44638b1)
    2.  [Magit](#org552826f)
    3.  [git-timemachine](#org9e8ee6b)
    4.  [swiper](#org5b1cb01)
    5.  [multiple-cursors](#org3608552)
    6.  [helm](#org90af900)
    7.  [expand-region](#org0827516)
    8.  [move-text](#orge558d6d)
    9.  [which-key](#orga47beeb)
    10. [doom-modeline](#org0e36474)
    11. [minimap](#org379ee3d)
8.  [EXWM](#orgdb05faa)
    1.  [Add wm](#org4b2fcef)
    2.  [exwm-package](#org6980e26)
    3.  [dmenu](#orge2d59d5)
    4.  [systemtray](#org474408d)
    5.  [randr](#org29394de)
    6.  [enable XF86](#org2c41b70)
    7.  [keybinds](#org19aae83)
    8.  [volume](#orga1c7810)
    9.  [default browser](#orgb52848f)
9.  [Org](#org3cf339b)
    1.  [Unbind](#org0b85b10)
    2.  [org-todo-keywords](#org400fd82)
    3.  [org-super-agenda](#org3f7b689)
    4.  [org-bullets](#org2cc97ac)
    5.  [tags](#org24f1aff)
    6.  [custom](#orgc48b6d0)
10. [Keybinds](#org256d6ad)
    1.  [end/start of buffer](#org9e5cd64)
    2.  [Switch buffers](#org3ec3ea6)
    3.  [org-capture](#orga4bd3a3)



<a id="org6a9155f"></a>

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


<a id="org5a6c80b"></a>

# Benchmark

    (use-package benchmark-init
     :ensure t)


<a id="orge47010d"></a>

# Basic


<a id="org763fd1b"></a>

## Annotate fix

    (eval-after-load "vc-git"
      '(defun vc-git-annotate-command (file buf &optional rev)
        (let ((name (file-relative-name file)))
          (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))
    )


<a id="org99e4c6a"></a>

## Skip gui

    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    
    (set-variable 'tooltip-mode nil)
    (blink-cursor-mode -1)
    (column-number-mode t)
    (setq inhibit-splash-screen t)


<a id="org7099dbd"></a>

## Enables show-paren-mode

    (setq show-paren-delay 0)
    (show-paren-mode 1)


<a id="orgfc9e5d6"></a>

## Auto-save and backup files saved in TEMP

    (setq backup-directory-alist
          `((".*" . ,"~/.saves")))
    (setq custom-file "~/.emacs.d/auto-custom.el")


<a id="orga8c969e"></a>

## Change all prompts to y or n

    (fset 'yes-or-no-p 'y-or-n-p)


<a id="org22aecf2"></a>

## settings for new frames (that is new window in windows, C-x 5 2)

    (setq default-frame-alist
          '((font . "Fira Mono")
            (vertical-scroll-bars . nil)
            (menu-bar-lines . 0)
            (left-fringe . 0)))


<a id="org8d89526"></a>

# Modes


<a id="orgb62572b"></a>

## js2-mode

    (use-package js2-mode
      :ensure t)
    
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode))
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))


<a id="org5e350c5"></a>

## nxml-mode

    (add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))


<a id="org61ba1eb"></a>

## web-mode

    (use-package web-mode
      :ensure t
      :mode ("\\.html\\'"))


<a id="org8c59562"></a>

## cc-mode

Switch between header and implementation file

    (eval-after-load "cc-mode"
      '(define-key c++-mode-map (kbd "C-x m") 'ff-find-other-file))


<a id="org73f12c5"></a>

# Functions


<a id="org3be4add"></a>

## Override alt-backspace

    (defun miker/backward-delete-word (arg)
      (interactive "p")
      (delete-region (point) (progn (backward-word arg) (point))))
    (global-set-key (kbd "M-<backspace>") 'miker/backward-delete-word)


<a id="orgeefa6ce"></a>

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


<a id="org816bc6a"></a>

## isearch-query-replace-symbol-at-point (M-s %)

    (defun miker/isearch-query-replace-symbol-at-point ()
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp)
    )
    (global-set-key (kbd "M-s %") 'miker/isearch-query-replace-symbol-at-point)


<a id="org1a4e8b8"></a>

# Look and feel


<a id="org065b3e0"></a>

## Theme


<a id="orgb65dd6b"></a>

### chocolate-theme

    ;;(set-background-color "wheat2")
    ;;(use-package chocolate-theme
    ;;  :ensure t
    ;;  :config
    ;;  (load-theme 'chocolate t))


<a id="org1e1f170"></a>

### dracula-theme

    (use-package dracula-theme
      :ensure t
      :config
      (load-theme 'dracula t))
    
    (set-face-attribute 'region nil :background "#342c6b" :foreground nil)


<a id="org6468a35"></a>

## Layout


<a id="org61eb36c"></a>

### Add line at top of the buffer to show column length

    (setq-default header-line-format
                  (list " " (make-string 76 ?-) "|"))

    ;;Highlight line
    ;;(global-hl-line-mode 1)
    ;;(set-face-background hl-line-face "darkgrey")
    (set-cursor-color "#C8FF03")


<a id="orgf177076"></a>

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


<a id="org34e07ad"></a>

### yascroll 'no scroll bar'

    ;; No scroll bar
    (set-face-background 'vertical-border "snow4")
    (set-face-foreground 'vertical-border (face-background 'vertical-border))
    
    (use-package yascroll
      :ensure t)
    (global-yascroll-bar-mode 1)


<a id="org948b17f"></a>

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


<a id="org5902300"></a>

### Trailing whitespaces

    (setq-default show-trailing-whitespace t)


<a id="org4532660"></a>

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


<a id="org6faac3a"></a>

### display-time-mode

    (setq display-time-24hr-format t)
    (setq display-time-format "(%H:%M %e/%m)")
    (display-time-mode 1)


<a id="org090af9a"></a>

## Input


<a id="orgb8ad939"></a>

### Insert matching delimiters

    (electric-pair-mode 1)
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


<a id="org4639cc3"></a>

### Indentation

    (setq-default indent-tabs-mode nil)
    (setq-default c-basic-offset 4)
    (setq-default js2-indent-level 4)
    (setq-default sgml-basic-offset 4)
    (setq-default cmake-tab-width 4)
    (setq-default nxml-child-indent 4 nxml-attribute-indent 4)


<a id="org5660ad7"></a>

### I-search

    (setq search-whitespace-regexp ".*?")


<a id="org2305f5f"></a>

# Packages


<a id="org44638b1"></a>

## Clang-format

    (use-package clang-format
      :ensure t
      :bind
      (("C-c f" . clang-format)))


<a id="org552826f"></a>

## Magit

    (use-package magit
      :ensure t
      :after (magit))


<a id="org9e8ee6b"></a>

## git-timemachine

    (use-package git-timemachine
      :ensure t
      :after (git-timemachine))


<a id="org5b1cb01"></a>

## swiper

    (use-package swiper
      :ensure t
      :bind
      ("C-c C-r" . swiper)
      )


<a id="org3608552"></a>

## multiple-cursors

    (use-package multiple-cursors
      :ensure t
      :bind
      ("C->" . mc/mark-next-like-this)
      ("C-<" . mc/mark-previous-like-this)
      ("C-c C-<" . 'mc/mark-all-like-this)
    )


<a id="org90af900"></a>

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


<a id="org0827516"></a>

## expand-region

    (use-package expand-region
      :ensure t
      :bind
      ("C-=" . er/expand-region)
      ("C-;" . er/expand-region))


<a id="orge558d6d"></a>

## move-text

    (use-package move-text
      :ensure t
      :bind
      (("C-S-p" . move-text-up)
       ("C-S-n" . move-text-down)))


<a id="orga47beeb"></a>

## which-key

    (use-package which-key
      :ensure t)
    (which-key-mode)


<a id="org0e36474"></a>

## doom-modeline

    (use-package all-the-icons)
    (use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-init)
    )


<a id="org379ee3d"></a>

## minimap

    (use-package minimap
      :ensure t
      :custom
      (minimap-window-location 'right)
      :config
      ;;dracula color
      (set-face-attribute 'minimap-active-region-background nil :background "#342c6b")
    )


<a id="orgdb05faa"></a>

# EXWM

tangle yes/no to enable/disable EXWM


<a id="org4b2fcef"></a>

## Add wm

    cd /usr/share/xsessions
    touch emacs.desktop

Add to emacs.desktop:

    [Desktop Entry]
    Name=EXWM
    Comment=Emacs window manager
    Exec=emacs
    Type=Application


<a id="org6980e26"></a>

## exwm-package

    (use-package exwm
      :ensure t
      :config
      (require 'exwm-config)
      (exwm-config-default)
    )


<a id="orge2d59d5"></a>

## dmenu

    (use-package dmenu
      :ensure t
      :bind
      ("s-SPC" . 'dmenu))


<a id="org474408d"></a>

## systemtray

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)


<a id="org29394de"></a>

## randr

    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist
    '(0 "DVI-D-0" 1 "DP-2"))
    (exwm-randr-enable)

Added in *home/<user>*.profile :
xrandr &#x2013;output DP-2 &#x2013;primary &#x2013;mode 2560x1440 &#x2013;rate 143.96
xrandr &#x2013;output DVI-D-0 &#x2013;mode 1920x1080 &#x2013;rotate right &#x2013;left-of DP-2

Run xrandr in term to see current settings


<a id="org2c41b70"></a>

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


<a id="org19aae83"></a>

## keybinds

    (global-set-key (kbd "s-k") 'exwm-workspace-delete)
    (global-set-key (kbd "s-w") 'exwm-workspace-swap)
    
    (global-set-key (kbd "<XF86ScreenSaver>") 'miker/launch-lock-screen)
    (global-set-key (kbd "<XF86PowerOff>") 'miker/launch-shutdown)


<a id="orga1c7810"></a>

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


<a id="orgb52848f"></a>

## default browser

    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "firefox")


<a id="org3cf339b"></a>

# Org


<a id="org0b85b10"></a>

## Unbind

C-, is reserved for switching buffers
C-c C-r is reserved for swiper

    (eval-after-load "org" '(define-key org-mode-map (kbd "C-,") nil))
    (eval-after-load "org" '(define-key org-mode-map (kbd "C-c C-r") nil))


<a id="org400fd82"></a>

## org-todo-keywords

    (setq org-todo-keywords
          '((sequence "TODO(t!)" "BUY(b!)" "BOOK(k!)" "LOOKUP(l!)" "|" "DONE(d!)")))


<a id="org3f7b689"></a>

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


<a id="org2cc97ac"></a>

## org-bullets

    (use-package org-bullets
     :ensure t
     :init
     (setq org-bullets-bullet-list
           '("ァ" "ィ" "ゥ" "ェ" "ォ"))
     :config
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


<a id="org24f1aff"></a>

## tags

    (setq org-tag-alist '(("@TRAVEL" . ?t)
                          ("@WEBSITE" . ?w)
                          ("KEYBOARD" . ?k)
                          ("@OREGON2020")
                          ))


<a id="orgc48b6d0"></a>

## custom

    (setq org-capture-templates '(
                                  ("t" "Todo")
                                  ("tt" "Todo" entry
                                   (file+headline
                                   "~/Documents/Private/todo.org" "Todo")
                                   "* TODO %?"
                                   :clock-in t :clock-resume t)
    
                                   ("tw" "Todo - Website" entry
                                   (file+headline
                                   "~/Documents/Private/todo.org" "Website")
                                   "* TODO %?")
    
                                  ("tr" "Todo - Travel" entry
                                   (file+headline
                                   "~/Documents/Private/todo.org" "Travel")
                                   "* BOOK %?")
    
                                  ("tp" "Todo - Purchase" entry
                                   (file+headline
                                   "~/Documents/Private/todo.org" "Purchase")
                                   "* BUY %?")
    
                                  ("tk" "Todo - Keyboard")
                                  ("tkk" "Todo - Keyboard" entry
                                   (file+headline
                                   "~/Documents/Private/todo.org" "Keyboard")
                                   "* TODO %?")
    
                                  ("tk6" "Todo - Keyboard - 65%" entry
                                   (file+olp
                                   "~/Documents/Private/todo.org" "Todo" "Keyboard" "65%")
                                   "* TODO %?")
    
                                  ("r" "Travel")
                                  ("ro" "Travel - Oregon2020" entry
                                   (file+headline
                                   "~/Documents/Private/todo.org" "Oregon2020")
                                   "* TODO %?")
                                 ))


<a id="org256d6ad"></a>

# Keybinds


<a id="org9e5cd64"></a>

## end/start of buffer

    (global-set-key (kbd "<end>") `end-of-buffer)
    (global-set-key (kbd "<home>") `beginning-of-buffer)


<a id="org3ec3ea6"></a>

## Switch buffers

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


<a id="orga4bd3a3"></a>

## org-capture

    (global-set-key (kbd "C-c c") `org-capture)

