;(setq debug-on-error t)

;; Some packages cause older emacs versions to crash and burn
(if (version< emacs-version "26.1")
    (require 'org)
  (org-babel-load-file
   (expand-file-name "emacs.org"
                     user-emacs-directory))
)

;make readme.md
;org-md-export-to-markdown
