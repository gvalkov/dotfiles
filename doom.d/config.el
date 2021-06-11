;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Georgi Valkov"
      user-mail-address "georgi.t.valkov@gmail.com")

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
(setq doom-font (font-spec :family "Input Mono" :size 18 :weight 'semi-light))
(setq doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-zenburn)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(remove-hook! 'text-mode-hook #'display-line-numbers-mode)


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

(setq confirm-kill-emacs nil)

(map! :n ", f" 'find-file)
(map! :n ", m" 'counsel-recentf)
(map! :n ", p" '+ivy/projectile-find-file)
(map! :n ", y" '+default/yank-pop)
(map! :n ", i" 'ibuffer)

(map! :n ", w" 'save-buffer)
(map! :n ", k" 'kill-this-buffer)
(map! :n ", d" 'kill-buffer-and-window)
(map! :n ", sc" 'doom/open-scratch-buffer)
(map! :n ", r" 'dired-jump)
(map! :n ", R" 'dired-jump-other-window)

(map! :n ", er" 'counsel-evil-registers)

(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(after! ivy
  (define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL")))

(after! dired
  (define-key dired-mode-map (kbd "e") (kbd "RET")))
;; (define-key dired-mode-map (kbd "C-h") 'dired-prev-subdir)
;; (define-key dired-mode-map (kbd "C-l") 'dired-next-subdir)

(use-package! salt-mode
  :defer t)

(use-package! jinja2-mode
  :mode (("\\.j2$" . jinja2-mode)
         ("\\.jinja" . jinja2-mode))
  :config
  (setq jinja2-enable-indent-on-save nil))

(map!
 :g "M-1"   #'winum-select-window-1
 :g "M-2"   #'winum-select-window-2
 :g "M-3"   #'winum-select-window-3
 :g "M-4"   #'winum-select-window-4
 :g "M-5"   #'winum-select-window-5
 :g "M-6"   #'winum-select-window-6
 :g "M-7"   #'winum-select-window-7
 :g "M-8"   #'winum-select-window-8
 :g "M-9"   #'winum-select-window-9
 :g "M-0"   #'winum-select-window-0-or-10)
