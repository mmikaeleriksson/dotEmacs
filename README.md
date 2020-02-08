
# Table of Contents

1.  [Package setup](#orgcd47f1b)
2.  [Benchmark](#org3f2f908)
3.  [Basic](#orgd9ddfcf)
    1.  [Annotate fix](#org9748b9d)
    2.  [Skip gui](#orgf8ab6b3)
    3.  [Enables show-paren-mode](#org5160e2b)
    4.  [Auto-save and backup files saved in TEMP](#orgaf442e6)
    5.  [Change all prompts to y or n](#org7f95997)
    6.  [settings for new frames (that is new window in windows, C-x 5 2)](#org2cac7e6)
4.  [Modes](#org1466887)
    1.  [rjsx-mode](#org9001a46)
    2.  [nxml-mode](#org8790c00)
    3.  [web-mode](#orgd392d75)
    4.  [cc-mode](#org657b5d1)
5.  [Functions](#orgebccca1)
    1.  [Override alt-backspace](#org1ed2e5d)
    2.  [Delete line (without kill-ring) (Ctrl-Shift-K)](#org957b6c0)
    3.  [isearch-query-replace-symbol-at-point (M-s %)](#org0a35940)
6.  [Look and feel](#org0a2a82e)
    1.  [Theme](#org22098ba)
        1.  [chocolate-theme](#orgbde440b)
        2.  [dracula-theme](#org3563db1)
    2.  [Layout](#org4787d82)
        1.  [Add line at top of the buffer to show column length](#org2325497)
        2.  [Fringe git-gutter settings](#org7124a64)
        3.  [yascroll 'no scroll bar'](#org690aba0)
        4.  [smart-mode-line](#org68a2f02)
        5.  [Trailing whitespaces](#org45f9fb6)
        6.  [Layout functions](#orgffcf718)
        7.  [display-time-mode](#org527373d)
    3.  [Input](#org44637e9)
        1.  [Insert matching delimiters](#org50a0067)
        2.  [Indentation](#orge33970e)
        3.  [I-search](#org8265938)
7.  [Packages](#org904fb15)
    1.  [Clang-format](#orgad42e0f)
    2.  [Magit](#org0c094d6)
    3.  [git-timemachine](#orgc5f94a7)
    4.  [swiper](#org96ced5c)
    5.  [multiple-cursors](#orge5a655e)
    6.  [helm](#orgcd6b268)
    7.  [expand-region](#orgac7d6ea)
    8.  [move-text](#org9d0cdf1)
    9.  [which-key](#org356305a)
    10. [doom-modeline](#orgad049f8)
    11. [minimap](#orgcf5acc3)
8.  [EXWM](#orgb328ab1)
    1.  [Add wm](#orgd90952d)
    2.  [exwm-package](#org8b1dbf5)
    3.  [dmenu](#orgb54874e)
    4.  [systemtray](#orgc4a1ca5)
    5.  [randr](#orgb9ae186)
    6.  [enable XF86](#orga2d56b5)
    7.  [keybinds](#org17bf1df)
    8.  [volume](#orgb3772fe)
    9.  [default browser](#orgecfc764)
9.  [Org](#orgaa6ac2c)
    1.  [Unbind](#orgaeb43e1)
    2.  [org-todo-keywords](#org678290b)
    3.  [org-super-agenda](#org8931179)
    4.  [org-bullets](#org5c3a95a)
    5.  [tags](#org6c58a6d)
    6.  [org-capture-templates](#org624a6b3)
10. [Keybinds](#org9125bdc)
    1.  [end/start of buffer (<home> | <end>)](#org1647e3d)
    2.  [Switch buffers (C-, | C-.)](#orga6626d6)
    3.  [org-capture (C-c c)](#org6db83a5)
    4.  [org-agenda (C-c a)](#orgba8199d)



<a id="orgcd47f1b"></a>

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


<a id="org3f2f908"></a>

# Benchmark

    (use-package benchmark-init
     :ensure t)


<a id="orgd9ddfcf"></a>

# Basic


<a id="org9748b9d"></a>

## Annotate fix

    (eval-after-load "vc-git"
      '(defun vc-git-annotate-command (file buf &optional rev)
        (let ((name (file-relative-name file)))
          (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))
    )


<a id="orgf8ab6b3"></a>

## Skip gui

    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    
    (set-variable 'tooltip-mode nil)
    (blink-cursor-mode -1)
    (column-number-mode t)
    (setq inhibit-splash-screen t)


<a id="org5160e2b"></a>

## Enables show-paren-mode

    (setq show-paren-delay 0)
    (show-paren-mode 1)


<a id="orgaf442e6"></a>

## Auto-save and backup files saved in TEMP

    (setq backup-directory-alist
          `((".*" . ,"~/.saves")))
    (setq custom-file "~/.emacs.d/auto-custom.el")


<a id="org7f95997"></a>

## Change all prompts to y or n

    (fset 'yes-or-no-p 'y-or-n-p)


<a id="org2cac7e6"></a>

## settings for new frames (that is new window in windows, C-x 5 2)

    (setq default-frame-alist
          '((font . "Fira Mono")
            (vertical-scroll-bars . nil)
            (menu-bar-lines . 0)
            (left-fringe . 0)))


<a id="org1466887"></a>

# Modes


<a id="org9001a46"></a>

## rjsx-mode

    (use-package rjsx-mode
      :ensure t)
    
    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
    (add-to-list 'interpreter-mode-alist '("node" . rjsx-mode))


<a id="org8790c00"></a>

## nxml-mode

    (add-to-list 'auto-mode-alist '("\\.ui\\'" . nxml-mode))


<a id="orgd392d75"></a>

## web-mode

    (use-package web-mode
      :ensure t
      :mode ("\\.html\\'"))


<a id="org657b5d1"></a>

## cc-mode

Switch between header and implementation file

    (eval-after-load "cc-mode"
      '(define-key c++-mode-map (kbd "C-x m") 'ff-find-other-file))


<a id="orgebccca1"></a>

# Functions


<a id="org1ed2e5d"></a>

## Override alt-backspace

    (defun miker/backward-delete-word (arg)
      (interactive "p")
      (delete-region (point) (progn (backward-word arg) (point))))
    (global-set-key (kbd "M-<backspace>") 'miker/backward-delete-word)


<a id="org957b6c0"></a>

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


<a id="org0a35940"></a>

## isearch-query-replace-symbol-at-point (M-s %)

    (defun miker/isearch-query-replace-symbol-at-point ()
    (interactive)
    (isearch-forward-symbol-at-point)
    (isearch-query-replace-regexp)
    )
    (global-set-key (kbd "M-s %") 'miker/isearch-query-replace-symbol-at-point)


<a id="org0a2a82e"></a>

# Look and feel


<a id="org22098ba"></a>

## Theme


<a id="orgbde440b"></a>

### chocolate-theme

    ;;(set-background-color "wheat2")
    ;;(use-package chocolate-theme
    ;;  :ensure t
    ;;  :config
    ;;  (load-theme 'chocolate t))


<a id="org3563db1"></a>

### dracula-theme

    (use-package dracula-theme
      :ensure t
      :config
      (load-theme 'dracula t))
    
    (set-face-attribute 'region nil :background "#342c6b" :foreground nil)


<a id="org4787d82"></a>

## Layout


<a id="org2325497"></a>

### Add line at top of the buffer to show column length

    (setq-default header-line-format
                  (list " " (make-string 76 ?-) "|"))

    ;;Highlight line
    ;;(global-hl-line-mode 1)
    ;;(set-face-background hl-line-face "darkgrey")
    (set-cursor-color "#C8FF03")


<a id="org7124a64"></a>

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


<a id="org690aba0"></a>

### yascroll 'no scroll bar'

    ;; No scroll bar
    (set-face-background 'vertical-border "snow4")
    (set-face-foreground 'vertical-border (face-background 'vertical-border))
    
    (use-package yascroll
      :ensure t)
    (global-yascroll-bar-mode 1)


<a id="org68a2f02"></a>

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


<a id="org45f9fb6"></a>

### Trailing whitespaces

    (setq-default show-trailing-whitespace t)


<a id="orgffcf718"></a>

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


<a id="org527373d"></a>

### display-time-mode

    (setq display-time-24hr-format t)
    (setq display-time-format "(%H:%M %e/%m)")
    (display-time-mode 1)


<a id="org44637e9"></a>

## Input


<a id="org50a0067"></a>

### Insert matching delimiters

    (electric-pair-mode 1)
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


<a id="orge33970e"></a>

### Indentation

    (setq-default indent-tabs-mode nil)
    (setq-default c-basic-offset 4)
    (setq-default js2-indent-level 4)
    (setq-default sgml-basic-offset 4)
    (setq-default cmake-tab-width 4)
    (setq-default nxml-child-indent 4 nxml-attribute-indent 4)


<a id="org8265938"></a>

### I-search

    (setq search-whitespace-regexp ".*?")


<a id="org904fb15"></a>

# Packages


<a id="orgad42e0f"></a>

## Clang-format

    (use-package clang-format
      :ensure t
      :bind
      (("C-c f" . clang-format)))


<a id="org0c094d6"></a>

## Magit

    (use-package magit
      :ensure t
      :after (magit))


<a id="orgc5f94a7"></a>

## git-timemachine

    (use-package git-timemachine
      :ensure t
      :after (git-timemachine))


<a id="org96ced5c"></a>

## swiper

    (use-package swiper
      :ensure t
      :bind
      ("C-c C-r" . swiper)
      )


<a id="orge5a655e"></a>

## multiple-cursors

    (use-package multiple-cursors
      :ensure t
      :bind
      ("C->" . mc/mark-next-like-this)
      ("C-<" . mc/mark-previous-like-this)
      ("C-c C-<" . 'mc/mark-all-like-this)
    )


<a id="orgcd6b268"></a>

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


<a id="orgac7d6ea"></a>

## expand-region

    (use-package expand-region
      :ensure t
      :bind
      ("C-=" . er/expand-region)
      ("C-;" . er/expand-region))


<a id="org9d0cdf1"></a>

## move-text

    (use-package move-text
      :ensure t
      :bind
      (("C-S-p" . move-text-up)
       ("C-S-n" . move-text-down)))


<a id="org356305a"></a>

## which-key

    (use-package which-key
      :ensure t)
    (which-key-mode)


<a id="orgad049f8"></a>

## doom-modeline

    (use-package all-the-icons)
    (use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-init)
    )


<a id="orgcf5acc3"></a>

## minimap

    (use-package minimap
      :ensure t
      :custom
      (minimap-window-location 'right)
      :config
      ;;dracula color
      (set-face-attribute 'minimap-active-region-background nil :background "#342c6b")
    )


<a id="orgb328ab1"></a>

# EXWM

tangle yes/no to enable/disable EXWM


<a id="orgd90952d"></a>

## Add wm

    cd /usr/share/xsessions
    touch emacs.desktop

Add to emacs.desktop:

    [Desktop Entry]
    Name=EXWM
    Comment=Emacs window manager
    Exec=emacs
    Type=Application


<a id="org8b1dbf5"></a>

## exwm-package

    (use-package exwm
      :ensure t
      :config
      (require 'exwm-config)
      (exwm-config-default)
    )


<a id="orgb54874e"></a>

## dmenu

    (use-package dmenu
      :ensure t
      :bind
      ("s-SPC" . 'dmenu))


<a id="orgc4a1ca5"></a>

## systemtray

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)


<a id="orgb9ae186"></a>

## randr

    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist
    '(0 "DVI-D-0" 1 "DP-2"))
    (exwm-randr-enable)

Added in *home/<user>*.profile :
xrandr &#x2013;output DP-2 &#x2013;primary &#x2013;mode 2560x1440 &#x2013;rate 143.96
xrandr &#x2013;output DVI-D-0 &#x2013;mode 1920x1080 &#x2013;rotate right &#x2013;left-of DP-2

Run xrandr in term to see current settings


<a id="orga2d56b5"></a>

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


<a id="org17bf1df"></a>

## keybinds

    (global-set-key (kbd "s-k") 'exwm-workspace-delete)
    (global-set-key (kbd "s-w") 'exwm-workspace-swap)
    
    (global-set-key (kbd "<XF86ScreenSaver>") 'miker/launch-lock-screen)
    (global-set-key (kbd "<XF86PowerOff>") 'miker/launch-shutdown)


<a id="orgb3772fe"></a>

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


<a id="orgecfc764"></a>

## default browser

    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "firefox")


<a id="orgaa6ac2c"></a>

# Org


<a id="orgaeb43e1"></a>

## Unbind

C-, is reserved for switching buffers
C-c C-r is reserved for swiper

    (eval-after-load "org" '(define-key org-mode-map (kbd "C-,") nil))
    (eval-after-load "org" '(define-key org-mode-map (kbd "C-c C-r") nil))


<a id="org678290b"></a>

## org-todo-keywords

    (setq org-todo-keywords
          '((sequence "TODO(t!)" "BUY(b!)" "BOOK(k!)" "LOOKUP(l!)" "|" "DONE(d!)")))


<a id="org8931179"></a>

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


<a id="org5c3a95a"></a>

## org-bullets

    (use-package org-bullets
     :ensure t
     :init
     (setq org-bullets-bullet-list
           '("ァ" "ィ" "ゥ" "ェ" "ォ"))
     :config
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


<a id="org6c58a6d"></a>

## tags

    (setq org-tag-alist '(("@TRAVEL" . ?t)
                          ("@WEBSITE" . ?w)
                          ("KEYBOARD" . ?k)
                          ("@OREGON2020")
                          ))


<a id="org624a6b3"></a>

## org-capture-templates

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


<a id="org9125bdc"></a>

# Keybinds


<a id="org1647e3d"></a>

## end/start of buffer (<home> | <end>)

    (global-set-key (kbd "<home>") `beginning-of-buffer)
    (global-set-key (kbd "<end>") `end-of-buffer)


<a id="orga6626d6"></a>

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


<a id="org6db83a5"></a>

## org-capture (C-c c)

    (global-set-key (kbd "C-c c") `org-capture)


<a id="orgba8199d"></a>

## org-agenda (C-c a)

    (global-set-key (kbd "C-c a") `org-agenda)

