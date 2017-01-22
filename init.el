;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   '(
     nginx
     better-defaults
     markdown
     emacs-lisp
     spell-checking
     lua
     git
     org
     go
     yaml
     html
     ruby
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     syntax-checking
     (auto-completion
      :variables
      auto-completion-enable-snippets-in-popup t
      auto-completion-private-snippets-directory (concat dotspacemacs-directory "snippets"))
     (python
      :variables
      python-test-runner 'pytest))

   dotspacemacs-additional-packages
   '(visual-regexp
     mark-multiple
     ibuffer-tramp
     ag
     dtrt-indent
     cmake-mode
     typescript-mode
     dedicated)

   dotspacemacs-excluded-packages
   '(;evil-jumper
     )

   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-editing-style   'vim
   dotspacemacs-verbose-loading  nil
   dotspacemacs-startup-banner  'nil
   dotspacemacs-startup-lists '(recents bookmarks projects)

   ;; Press <leader> T n to cycle to the next theme in the list.
   dotspacemacs-themes '(zenburn
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai)

   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ";"
   dotspacemacs-major-mode-emacs-leader-key "C-;"
   dotspacemacs-command-key ":"

   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-auto-save-file-location 'cache

   ;; Helm
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom

   ;; UI
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   ))

;;;---------------------------------------------------------------------------
;;; User Config
;;;---------------------------------------------------------------------------
(defconst is-bsd (eq system-type 'berkeley-unix))
(defconst is-linux (eq system-type 'gnu/linux))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."

  (setq-default git-magit-status-fullscreen t))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  ;;--------------------------------------------------------------------------
  (add-to-list 'load-path (concat dotspacemacs-directory "/lisp"))
  (require 'my-defuns)
  (require 'my-config-defuns)

  ;;--------------------------------------------------------------------------
  ;; backup, autosave and lockfiles
  (setq backup-directory-alist `((".*" . ,(concat spacemacs-cache-directory "/backups/"))))
  ;; (setq auto-save-file-name-transforms `((".*", "~/.emacs.d/backups/" t))
  (setq version-control t
        make-backup-files t
        backup-by-copying nil
        delete-by-moving-to-trash nil
        delete-old-versions t
        kept-old-versions 5
        kept-new-versions 9
        auto-save-timeout 20
        auto-save-interval 200
        create-lockfiles t)

  ;;--------------------------------------------------------------------------
  ;; clipboard
  (setq x-select-enable-clipboard t
        x-select-enable-primary t
        save-interprogram-paste-before-kill t
        mouse-yank-at-point t)

  ;;--------------------------------------------------------------------------
  ;; ui
  (setq powerline-default-separator 'nil)
  (setq scroll-step 2)
  (setq scroll-conservatively 4)
  ;; (setq split-height-threshold 110)
  ;; (setq enable-recursive-minibuffers nil)
  (setq mouse-wheel-follow-mouse t
        mouse-wheel-scroll-amount '(1 ((shift) . 1)))

  ;;--------------------------------------------------------------------------
  ;; editor
  (setq-default tab-width 4
                c-basic-offset 4
                indent-tabs-mode nil
                tab-always-indent t)

  (setq require-final-newline t)

  ;;--------------------------------------------------------------------------
  ;; mode customizaitons
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 40)

  ;; dired -------------------------------------------------------------------
  (setq-default dired-listing-switches (if is-bsd "-alh" "-alhv"))
  (setq dired-recursive-deletes 'top)
  (setq dired-recursive-copies 'top)
  (setq dired-dwim-target t)

  (setq dired-auto-revert-buffer t)

  ;; company -----------------------------------------------------------------
  (setq company-show-numbers t)

  ;; tramp -------------------------------------------------------------------
  (setq password-cache-expiry nil)
  (setq tramp-verbose 6)
  (setq tramp-ssh-controlmaster-options
        (s-join " "
                `("-o ControlMaster=yes"
                  "-o ControlPersist=yes"
                  ,(format "-oControlPath='%s/ssh.%%%%r@%%%%h:%%%%p'" spacemacs-cache-directory))))

  ;; ibuffer -----------------------------------------------------------------
  (spacemacs|use-package-add-hook ibuffer
    :post-config
    (progn
      (define-ibuffer-column size-human
        (:name "Size" :inline t)
        (cond
         ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
         ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
         (t (format "%8d" (buffer-size)))))

      (add-hook! 'ibuffer-mode-hook
                 (progn
                   (ibuffer-tramp-set-filter-groups-by-tramp-connection)
                   (ibuffer-tramp-set-filter-groups-by-tramp-connection)
                   (ibuffer-do-sort-by-alphabetic)))))

  ;; flycheck ----------------------------------------------------------------
  (setq flycheck-check-syntax-automatically '(save))

  ;; magit -------------------------------------------------------------------
  (spacemacs|use-package-add-hook magit
    :post-config
    (progn
      (setq magit-push-always-verify t
            magit-process-popup-time -1
            magit-diff-refine-hunk nil
            magit-repository-directories '("~/source/github" "~/source/work" "~/source/misc"))))

  ;; python ------------------------------------------------------------------
  (spacemacs|use-package-add-hook python
    :post-config
    (add-hook! 'python-mode-hook
               (setq indent-tabs-mode t
                     python-indent-offset 4
                     tab-width 4)))

  ;; helm --------------------------------------------------------------------
  ;; (eval-after-load 'helm
  ;;   (add-to-list 'helm-boring-buffer-regexp-list
  ;;                (rx (or
  ;;                     "*Ibuffer*"
  ;;                     "*Help*"
  ;;                     "*Completions*"
  ;;                     "*Customize")))
  ;;   (define-key helm-map (kbd "<f11>") 'my/helm-fullscreen-toggle))

  ;; mark-multiple -----------------------------------------------------------
  (use-package mark-multiple
    :init
    (progn
      (define-key evil-visual-state-map "]" 'mark-next-like-this)
      (define-key evil-visual-state-map "[" 'mark-previous-like-this)
      (define-key evil-visual-state-map "m" 'mark-more-like-this)))

  ;; calendar ----------------------------------------------------------------
  (setq calendar-week-start-day 1
        calendar-date-style 'european
        calendar-mark-holidays-flag nil)

  ; add week numbers
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-warning-face))
  (setq calendar-intermonth-header
        (propertize "Wk" 'font-lock-face 'font-lock-keyword-face))

  ;; org ----------------------------------------------------------------------
  (with-eval-after-load 'org
    (setq org-startup-folded 0
          org-cycle-separator-lines 2))

  ;; org-babel ----------------------------------------------------------------
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh . t)
     (plantuml . t)
     (dot .t )
     (ledger . t)
     (python . t)))

  ;; -------------------------------------------------------------------------
  (use-package dedicated
    :bind ("C-x d" . dedicated-mode))

  (use-package dtrt-indent
	:init
	(progn
	  (add-hook 'prog-mode-hook 'dtrt-indent-mode)))

  ;;--------------------------------------------------------------------------
  ;; custom toggles
  (spacemacs|add-toggle auto-save-on-focus-out
    :on (add-hook 'focus-out-hook 'my/save-buffer-if-needed)
    :off (remove-hook 'focus-out-hook 'my/save-buffer-if-needed)
    :status (member 'my/save-buffer-if-needed focus-out-hook)
    :documentation "Auto-save on focus-out."
    :evil-leader "tS")

  ;;--------------------------------------------------------------------------
  ;; shortcuts
  (global-set-key [down-mouse-3] 'x-menu-bar-open)
  (global-set-key (kbd "<menu>") 'x-menu-bar-open)
  (global-set-key (kbd "C-:") 'eval-expression)

  (global-set-key (kbd "<f12>") 'spacemacs/toggle-whitespace)
  (global-set-key (kbd "C-c n") 'my/show-and-copy-file-name)

  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)

  (bind-map my/quick-leader-map
    :evil-keys (",")
    :evil-states (normal visual motion hybrid emacs))

  (bind-map-set-keys my/quick-leader-map
    ;; helm shortcuts
    "f"  'helm-find-files
    "m"  'helm-recentf
    "l"  'helm-mini
    "p"  'helm-projectile
    "y"  'helm-show-kill-ring
    "sw" 'helm-swoop
    "b"  'ibuffer

    ;; query/replace
    "vr" 'vr/replace
    "vq" 'vr/query-replace

    ;; files and buffers
    "w" 'save-buffer
    "k" 'kill-this-buffer
    "d" 'kill-buffer-and-window
    "q" 'kill-buffer-and-window
    "sc" (lambda () (interactive) (switch-to-buffer "*scratch*"))

    "h" 'ff-find-other-file
    "u" 'undo-tree-visualize
    "r" 'dired-jump
    "i" 'helm-semantic-or-imenu
    "er" 'evil-show-registers

    "g" (lookup-key spacemacs-default-map (kbd "g"))
    )

  ;; misc
  (setq-default buffers-menu-max-size 20)
  (setq-default compilation-scroll-output 'first-error)
  ;; (setq indicate-buffer-boundaries 'left)

  ;; compatibility vars and defuns
  (setq-default redisplay-dont-pause t)
  (evil-define-key 'normal evil-jumper-mode-map (kbd "TAB") nil)

  ;;--------------------------------------------------------------------------
  ;; hooks
  ;; (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'text-mode-hook 'enable-hard-wrap)
  (add-hook 'prog-mode-hook 'enable-comment-hard-wrap)

  ;;--------------------------------------------------------------------------
  ;; site-specific elisp
  (let ((site-local (concat dotspacemacs-directory "site-local.el")))
    (when (file-readable-p site-local) (load site-local)))
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol t)
 '(package-selected-packages
   (quote
    (memory-usage nginx-mode yapfify uuidgen rake py-isort pug-mode org-projectile pcache org org-download mwim minitest live-py-mode link-hint hide-comnt go-guru git-link flyspell-correct-helm flyspell-correct eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff goto-chg undo-tree dumb-jump diminish column-enforce-mode go-eldoc company-go go-mode helm-flyspell auto-dictionary typescript-mode rpm-spec-mode ag web-mode tagedit slim-mode scss-mode sass-mode less-css-mode jade-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data yaml-mode ws-butler window-numbering volatile-highlights visual-regexp vi-tilde-fringe toc-org spacemacs-theme spaceline powerline smooth-scrolling smeargle rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-delimiters pyvenv pytest pyenv-mode py-yapf popwin pip-requirements persp-mode pcre2el paradox hydra spinner page-break-lines orgit org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-bullets open-junk-file neotree move-text mmm-mode markdown-toc markdown-mode mark-multiple magit-gitflow macrostep lua-mode lorem-ipsum linum-relative leuven-theme info+ indent-guide ido-vertical-mode ibuffer-tramp ibuffer-projectile hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile helm-gitignore request helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md flycheck-pos-tip flycheck pkg-info epl flx-ido flx fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-args evil-anzu anzu eval-sexp-fu highlight elisp-slime-nav dtrt-indent define-word dedicated cython-mode company-statistics company-quickhelp pos-tip company-anaconda company cmake-mode clean-aindent-mode chruby bundler inf-ruby buffer-move bracketed-paste auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup quelpa package-build use-package which-key bind-key bind-map evil zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
