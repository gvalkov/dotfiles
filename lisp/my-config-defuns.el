;;-----------------------------------------------------------------------------
;; functions and macros related to configuration
;;-----------------------------------------------------------------------------

(defmacro λ (&rest body)
  `(lambda () (interactive) ,@body))

(defmacro associate-mode (regex mode)
  `(add-to-list 'auto-mode-alist '(,regex . ,mode)))

(defmacro associate-minor-mode (regex minor-mode)
  `(add-to-list 'auto-minor-mode-alist '(,regex . ,minor-mode)))

(defmacro add-hook! (hook &rest body)
  `(add-hook ,hook (lambda() ,@body)))

(defmacro add-hooks! (hooks &rest body)
  `(let ((fun #'(lambda() ,@body)))
     (dolist (hook ,hooks)
       (add-hook hook fun))))
 
(defmacro evil-defmap (map &rest body)
  `(evil-define-key nil ,map ,@body))

(defun add-plugin (p)
  (add-to-list 'load-path (concat basedir "plugins/" p )))

(defun add-plugin-dir (p)
  (dolist (f (directory-files p))
    (let ((name (concat p f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

(defmacro after (feature &rest forms)
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "after: cannot find %s" feature)
       'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defun enable-comment-hard-wrap ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode 1))

(defun enable-hard-wrap ()
  (auto-fill-mode 1))

(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun map-add-to-list (dest &rest args)
  (mapc '(lambda (arg) (add-to-list dest arg))
        args))

(provide 'my-config-defuns)
