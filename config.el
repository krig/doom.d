;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name (or (getenv "NAME") "Kristoffer Grönlund")
      user-mail-address (or (getenv "EMAIL") "k@ziran.se"))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "Fira Code" :size 15))
(setq doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)


(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)



;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; general options
(setq-default
 default-directory "~"
 comment-style 'indent
 delete-by-moving-to-trash t
 uniquify-buffer-name-style 'forward
 window-combination-resize t
 truncate-string-ellipsis "…"
 auto-save-default t
 line-spacing nil
 calendar-week-start-day 1
 avy-all-windows t)

;; vterm options
(setq
 vterm-always-compile-module t)

;; set indents
(setq-default
 tab-width 4
 js-indent-level 2
 typescript-indent-level 2
 css-indent-offset 2)

;; CamelCase is a single word
(global-subword-mode 1)

;; default list of projects
(setq projectile-project-search-path '("~/src/"))

;; start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(fset 'yes-no-or-p 'y-or-n-p)

;; automatically revert a file if it hasn't been changed in the editor but
;; changes on disk
(global-auto-revert-mode t)

;; Automatically compress/decompress files
(auto-compression-mode 1)

;; delete selection when adding new text (like most editors)
(delete-selection-mode t)

(map! :ne "M-/" #'comment-or-uncomment-region)

;; ask for buffer when splitting
;; from https://tecosaur.github.io/emacs-config/config.html
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
(setq +ivy-buffer-preview t)

;; various functions
(load! "functions.el")

(setq initial-major-mode 'markdown-mode)
(setq initial-scratch-message "# Scratchpad")

;; indentation support for sql-mode
;(use-package! sql-indent
;  :hook (sql-mode . sqlind-minor-mode)
;  :init
;  (setq sqlind-basic-offset 4))

;; simpler indentation mode for SQL
(use-package! sql-smie-mode
  :hook (sql-mode . sql-smie-mode))

;; jinja2 templates in SQL
(use-package! jinja2-minor-mode
  :hook (sql-mode . jinja2-minor-mode))

(setq-default olivetti-body-width 120)
(use-package! olivetti
  :hook (markdown-mode . olivetti-mode))

(add-hook! zig-mode
  (setq zig-format-on-save nil))

(solaire-global-mode +1)

;; make _ part of words in python
(add-hook! python-mode-hook (modify-syntax-entry ?_ "w"))

(add-hook! python-mode
  (setq python-shell-interpreter "python3"))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
