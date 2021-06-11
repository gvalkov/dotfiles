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
   '(perl5
     rust
     javascript
     vimscript
     nginx
     ansible
     better-defaults
     markdown
     emacs-lisp
     systemd
     docker
     spell-checking
     typescript
     clojure
     ivy
     lua
     nim
     git
     org
     windows-scripts
     (go :variables go-backend 'go-mode)
     yaml
     html
     c-c++
     puppet
     ruby
     windows-scripts
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
     groovy-mode
     coffee-mode
     ninja-mode
     dtrt-indent
     cmake-mode
     typescript-mode
     dedicated
     )

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

   dotspacemacs-mode-line-theme 'spacemacs
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Input Mono"
                               :size 18
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

   dotspacemacs-elpa-timeout 20

   ;; Helm
   ;; dotspacemacs-use-ido nil
   ;; dotspacemacs-helm-resize nil
   ;; dotspacemacs-helm-no-header t
   ;; dotspacemacs-helm-position 'bottom

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
  (require 'org-protocol)

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
        create-lockfiles t
        focus-follows-mouse t)

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

  ;; useless buffers
  (setq spacemacs-useless-buffers-regexp '("\\*\.\+\\*" "TAGS"))

  ;; useless buffers
  (setq spacemacs-useless-buffers-regexp '("\\*\.\+\\*" "TAGS"))

  ;;--------------------------------------------------------------------------
  ;; editor
  (setq-default tab-width 4
                c-basic-offset 4
                indent-tabs-mode nil
                tab-always-indent t)

  (setq require-final-newline t)

  ;;--------------------------------------------------------------------------
  ;; browser
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox")

  ;;--------------------------------------------------------------------------
  ;; mode customization
  (setq recentf-max-saved-items 1000)
  (setq recentf-max-menu-items 50)
  (setq recentf-auto-cleanup 'never)
  ;; (setq recentf-auto-save-timer (run-with-idle-timer 60 t 'recentf-save-list))

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
  ;; (setq tramp-ssh-controlmaster-options nil)
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
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled)
        flycheck-python-pycompile-executable "python3")

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
               (setq indent-tabs-mode nil
                     python-indent-offset 4
                     tab-width 4)))

  ;; dns ---------------------------------------------------------------------
  (setq dns-mode-soa-auto-increment-serial nil)

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
          org-cycle-separator-lines 1
          org-agenda-span 31
          org-agenda-files '("~/org/" "~/org/prog")
          org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))

    (add-to-list 'org-modules 'org-protocol)
    (setq org-capture-templates '(
                                  ("t" "Tool link" entry (file+headline "~/org/prog/tools.org" "Unsorted")
                                   "* [[%x][%?]]")
                                  ("n" "Note" entry (file+olp+datetree "~/org/notes.org" "Бележки")
                                   "* %?]")
                                  ("p" "Protocol" entry (file+headline "~/org/inbox.org" "Inbox")
                                   "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                  ("L" "Protocol Link" entry (file+headline "~/org/inbox.org" "Inbox")
                                   "* %? [[%:link][%:description]] \nCaptured On: %U")
                                  )))

  (defadvice org-switch-to-buffer-other-window
      (after supress-window-splitting activate)
    "Delete the extra window if we're in a capture frame"
    (if (equal "org-protocol-capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defun gv/post-capture ()
    (if (equal "org-protocol-capture" (frame-parameter nil 'name))
        (delete-frame)))
  (add-hook 'org-capture-after-finalize-hook 'gv/post-capture)

  (defun gv/org-ispell ()
    "Configure `ispell-skip-region-alist' for `org-mode'."
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
  (add-hook 'org-mode-hook #'gv/org-ispell)


  ;; org-babel ----------------------------------------------------------------
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
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

  (bind-map my/quick-leader-map
    :evil-keys (",")
    :evil-states (normal visual motion hybrid emacs))

  (bind-map-set-keys my/quick-leader-map
    ;; helm shortcuts
    "f"  'find-file
    "m"  'counsel-recentf
    "l"  'ivy-switch-buffer
    "p"  'counsel-projectile
    "y"  'counsel-yank-pop
    "sw" 'swiper
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

  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-substitute)

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
  (add-hook 'text-mode-hook 'spacemacs/toggle-auto-fill-mode-off)
  (add-hook 'text-mode-hook 'spacemacs/toggle-truncate-lines-off)
  (add-hook 'prog-mode-hook 'enable-comment-hard-wrap)

  ;;--------------------------------------------------------------------------
  ;; site-specific elisp
  (let ((site-local (concat dotspacemacs-directory "site-local.el")))
    (when (file-readable-p site-local) (load site-local)))
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cmake-tab-width 4)
 '(evil-want-Y-yank-to-eol t)
 '(package-selected-packages
   (quote
    (powershell request parent-mode flx anzu web-completion-data sesman pkg-info bind-map auto-complete popup docker tablist dockerfile-mode docker-tramp clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider seq queue clojure-mode tide toml-mode racer flycheck-rust cargo rust-mode wgrep smex ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy gitignore-mode epl org-mime ninja-mode groovy-mode web-beautify livid-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern tern coffee-mode dash-functional iedit flyspell-correct ghub let-alist pythonic disaster company-c-headers clang-format impatient-mode skewer-mode org-category-capture haml-mode geiser yapfify yaml-mode web-mode visual-regexp typescript-mode ruby-test-mode rubocop rspec-mode robe pyvenv orgit org-projectile nginx-mode mmm-mode markdown-toc markdown-mode magit-gitflow live-py-mode ibuffer-projectile helm-company go-guru go-eldoc git-timemachine git-link flycheck-pos-tip flycheck company-go go-mode chruby ag inf-ruby anaconda-mode company magit magit-popup git-commit with-editor yasnippet alert log4e which-key use-package toc-org spaceline restart-emacs persp-mode org-plus-contrib neotree link-hint info+ indent-guide hungry-delete highlight-indentation hide-comnt help-fns+ helm-projectile helm-make projectile helm-flx eyebrowse expand-region exec-path-from-shell evil-search-highlight-persist evil-nerd-commenter evil-mc evil-exchange dumb-jump diminish aggressive-indent adaptive-wrap ace-window ace-link avy packed highlight smartparens f s dash evil helm helm-core async hydra zenburn-theme ws-butler winum volatile-highlights vimrc-mode vi-tilde-fringe uuidgen unfill undo-tree tagedit smeargle slim-mode scss-mode sass-mode rvm ruby-tools rbenv rake rainbow-delimiters pytest pyenv-mode py-isort puppet-mode pug-mode powerline pos-tip popwin pip-requirements pcre2el paradox org-present org-pomodoro org-download org-bullets open-junk-file mwim move-text minitest mark-multiple macrostep lua-mode lorem-ipsum linum-relative less-css-mode jinja2-mode ibuffer-tramp hy-mode htmlize hl-todo highlight-parentheses highlight-numbers helm-themes helm-swoop helm-pydoc helm-mode-manager helm-gitignore helm-descbinds helm-css-scss helm-c-yasnippet helm-ag goto-chg google-translate golden-ratio gnuplot gntp gitconfig-mode gitattributes-mode git-messenger gh-md fuzzy flyspell-correct-helm flx-ido fill-column-indicator fancy-battery evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-numbers evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav dtrt-indent define-word dedicated dactyl-mode cython-mode company-web company-statistics company-ansible company-anaconda column-enforce-mode cmake-mode clean-aindent-mode bundler bind-key auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile ansible-doc ansible ace-jump-helm-line ac-ispell)))
 '(python-shell-interpreter "python3" t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
