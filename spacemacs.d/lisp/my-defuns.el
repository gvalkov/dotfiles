(defun my/helm-fullscreen-toggle ()
  (interactive)
  ()
  (with-selected-window (helm-window)
    (delete-other-windows)))

(defun my/reload-emacs-config ()
  (interactive)
  (let ((init-el  "~/.emacs.d/init.el"))
    (and (get-file-buffer init-el)
         (save-buffer (get-file-buffer init-el)))
    (load-file init-el)))

(defun my/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p
         (format "Really delete '%s'?" (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun my/org-calendar-buffer ()
  "Open an org schedule calendar in the new buffer."
  (interactive)
  (let* ((source1 (cfw:org-create-source))
         (my-buffer (get-buffer-create cfw:calendar-buffer-name))
         ;; (frame-background-mode "light")
         (cp (cfw:create-calendar-component-buffer
              :view 'day
              :buffer my-buffer
              :contents-sources (list source1)
              :custom-map cfw:org-schedule-map
              :sorter 'cfw:org-schedule-sorter)))
    (load-theme-buffer-local 'sanityinc-solarized-light my-buffer)
    (cfw:cp-get-buffer cp)))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected
  and current line is not blank and we are not at the end of the
  line, then comment current line.  Replaces default behaviour of
  comment-dwim, when it inserts comment at the end of the line. If we
  are commenting a full line, we also yank its content to the 'c'
  register."

  (interactive "*P")
  (comment-normalize-vars)

  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (unless (comment-only-p beg end) (copy-to-register ?1 beg end))

    (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
        (comment-or-uncomment-region beg end)
      (comment-dwim arg))))

(defun my/toggle-linum-keycb (fmt)
  (lexical-let ((format fmt))
    (lambda ()
      (interactive)
      (if linum-mode
          (if (eq linum-format format)
              (command-execute 'linum-mode)
            (setq linum-format format))
        (progn
          (setq linum-format format)
          (command-execute 'linum-mode))))))

;-----------------------------------------------------------------------------
; https://gist.github.com/nibrahim/640311
(defun my/sort-lines-by-length (b e)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region b e)
      (let ((items (sort
                    (split-string
                     (buffer-substring (point-min) (point-max)) "[\n]")
                    (lambda(x y) (< (length x) (length y))))))
        (delete-region (point-min) (point-max))
        (save-excursion
          (point-min)
          (insert (apply 'concat (map 'list (lambda (x) (format "%s\n" x)) items))))))))

(defun my/reverse-words (b e)
  "Reverse the order of words in region."
  (interactive "*r")
  (apply
   'insert
   (reverse
    (split-string
     (delete-and-extract-region b e) "\\b"))))

;-----------------------------------------------------------------------------
(defun my/quote-word-region-to-list (b e)
  (interactive "r")
  (save-excursion
    (set-mark-command nil)
    (save-restriction
      (narrow-to-region b e)
      (beginning-of-buffer)
      (while (search-forward-regexp "\\(\\b[a-zA-Z0-9_$]+\\b\\)" nil t )
        (replace-match "'\\1',")))
    (setq deactivate-mark nil)))

(defun my/swap-dictionary-keys (b e)
  (interactive "r")
  (save-excursion
    (set-mark-command nil)
    (save-restriction
      (narrow-to-region b e)
      (beginning-of-buffer)

      (while (search-forward-regexp "^\\( +\\)\\(.*?\\):\\( *\\)\\(.*?\\)," nil t )
        (replace-match "\\1\\4: \\2,")))
    (setq deactivate-mark nil)))

;-----------------------------------------------------------------------------
(defun align= (b e)
  (interactive "r")
  (align-regexp b e "\\(\\s-*\\)[=|:]" 1 1))

(defun align: (b e)
  (interactive "r")
  (align-regexp b e ":\\(\\s-*\\)" 1 1 nil))

(defun align-lstrip-untabify: (b e)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region b e)
      (untabify (point-min) (point-max))
      (goto-char (point-min))
      (while (re-search-forward " +:" nil t)
        (replace-match ": "))
      (align-regexp (point-min) (point-max) "\\(\\s-+\\)")
      (goto-char (point-min)))))

;-----------------------------------------------------------------------------
(defun my/untabify-trailing (b e)
  (interactive "r")
  (save-excursion
    (goto-char b)
    (while (< (point) e)
      (beginning-of-line)
      (skip-chars-forward " 	")
      (untabify (point) (line-end-position))
      (forward-line 1))))

;-----------------------------------------------------------------------------
; "borrowed" from github.com/hlissner's emacs config
(defun my/select-previous-evil-paste ()
  (interactive)
  (evil-goto-mark ?\[)
  (evil-visual-char)
  (evil-goto-mark ?\]))

(defun my/newline-and-indent ()
  "Newline and indent; if in a comment, auto-comment and properly
indent the next line."
  (interactive)
  (cond ((sp-point-in-string)
         (evil-ret))
        ((evil-in-comment-p)
         (indent-new-comment-line))
        (t
         (evil-ret-and-indent))))

(defun my/minibuffer-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is
active, just deactivate it; then it takes a second \\[keyboard-quit]
to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;-----------------------------------------------------------------------------
(defun my/show-and-copy-file-name ()
  "Show and copy the full path name. If tramp buffer, the local
name of the file and the full buffer name will be pushed to the
kill ring."
  (interactive)
    (let* ((vec (ignore-errors (tramp-dissect-file-name (buffer-file-name))))
           (local-name (when vec (elt vec 3)))
           (buffer-name (file-truename buffer-file-name)))
      (message buffer-name)
      (when local-name
        (kill-new local-name))
      (kill-new buffer-name)))

(defun my/url-insert-file-contents (url)
  "Prompt for URL and insert file contents at point."
  (interactive "sURL: ")
  (url-insert-file-contents url))

(defun my/save-buffer-if-needed ()
  (interactive)
  (when (and (buffer-file-name) (buffer-modified-p))
    (save-buffer)))

;-----------------------------------------------------------------------------
; Skip uninteresing buffers.
; http://stackoverflow.com/questions/14323516/make-emacs-next-buffer-skip-messages-buffer
(defun my/change-buffer (change-buffer)
  "Call CHANGE-BUFFER until current buffer is not in `my-boring-buffers'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (member (buffer-name) my/boring-buffers)
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

(defun my/next-buffer ()
  "`next-buffer' that skips `my/boring-buffers'."
  (interactive)
  (my/change-buffer 'next-buffer))

(defun my/previous-buffer ()
  "`previous-buffer' that skips `my/boring-buffers'."
  (interactive)
  (my/change-buffer 'previous-buffer))

;;-----------------------------------------------------------------------------
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;;-----------------------------------------------------------------------------
(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun git-amend-force-push ()
  (interactive)
  (shell-command "git add -u && git commit --amend --no-edit && git push origin HEAD --force"))

;-----------------------------------------------------------------------------
; Org mode

;-----------------------------------------------------------------------------
; Copy-Pasta
;-----------------------------------------------------------------------------

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun my/region-to-snippet (begin end)
  "Write new snippet based on current region."
  (interactive "r")
  (let ((region (buffer-substring begin end)))
    (yas-new-snippet)
    (save-excursion
      (goto-char (point-max))
      (insert region))))

(provide 'my-defuns)
