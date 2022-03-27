(defun bh:set-whitespace-defaults()
  ; only save the values the first time we get here
  (unless (boundp 'bh:default-whitespace-style)
    (setq bh:default-whitespace-style                (default-value 'whitespace-style)
          bh:default-whitespace-display-mappings     (default-value 'whitespace-display-mappings)
          bh:doom-whitespace-style                   whitespace-style
          bh:doom-whitespace-display-mappings        whitespace-display-mappings
          bh:whitespace-mode                         "doom")))

; whitespace-style etc are set up with default-values in whitespace.el and then
; modified in doom-highlight-non-default-indentation-h (in core/core-ui.el).
; This is added to after-change-major-mode-hook in doom-init-ui-h (in
; core/core-ui.el) and called a LOT: so I need to capture doom's modified
; settings after that. The trouble is, this file (config.el) is called _before_
; doom-init-ui-h which is called in window-setup-hook as the last gasp of
; doom-initialize! find-file-hook appears to work.
(add-hook 'find-file-hook #'bh:set-whitespace-defaults 'append)

; doom=>default=>off=>doom=>...
(defun bh:toggle-whitespace () (interactive)
       (cond ((equal bh:whitespace-mode "doom")
              (setq whitespace-style bh:default-whitespace-style
                    whitespace-display-mappings bh:default-whitespace-display-mappings
                    bh:whitespace-mode "default")
              (prin1 (concat "whitespace-mode is whitespace default"))
              (whitespace-mode))
             ((equal bh:whitespace-mode "default")
              (setq bh:whitespace-mode "off")
              (prin1 (concat "whitespace-mode is off"))
              (whitespace-mode -1))
             (t ; (equal bh:whitespace-mode "off")
              (setq whitespace-style bh:doom-whitespace-style
                    whitespace-display-mappings bh:doom-whitespace-display-mappings
                    bh:whitespace-mode "doom")
              (prin1 (concat "whitespace-mode is doom default"))
              (whitespace-mode))))

(global-set-key (kbd "C-<f4>")          'bh:toggle-whitespace)
