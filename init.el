(defvar dotemacs/dir "~/.emacs.d/lisp")

(unless (file-exists-p dotemacs/dir)
  (dired-create-directory dotemacs/dir))

(add-to-list 'load-path dotemacs/dir)

(when (file-exists-p (expand-file-name "dotemacs.el" dotemacs/dir))
  (require 'dotemacs))
