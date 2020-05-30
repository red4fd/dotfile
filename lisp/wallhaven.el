;; wallhaven pictures -*- lexical-binding: t -*-
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Unloading.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-HTML_002fXML.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Document-Object-Model.html#Document-Object-Model
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html#Association-Lists
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/dolist.html

(require 'sgml-mode)

(defvar wallhaven-cache-dir (expand-file-name "wallhaven/" user-emacs-directory))
(defvar pic-html-name (expand-file-name "demo.html" wallhaven-cache-dir))
(defvar pic-dir-filename "~/Pictures/wallhaven")
(defvar pic-urls-cache-filename (expand-file-name "urls" wallhaven-cache-dir))

;; 下载
(defun com-pic-url (name type)
  (let ((full-name (concat "wallhaven-" name type)))
    (concat "https://w.wallhaven.cc/full/"
	    (substring name 0 2) "/"
	    full-name)))

(defun get-picture (name style)
  "下载图片"
  (let ((full-name (concat "wallhaven-" name style)))
    (when (file-exists-p full-name)
      (delete-file full-name))
    (url-copy-file (concat "https://w.wallhaven.cc/full/"
			   (substring name 0 2) "/"
			   full-name)
		   full-name)))

;; 解析

(defun get-dom (file-name)
  "得到 dom 对象"
  (find-file file-name)
  (let ((dom (with-current-buffer file-name
	       (libxml-parse-html-region (point-min) (point-max)))))
    (kill-buffer file-name)
    dom))

(defun get-all-pic-dom ()
  "得到 li dom 集合"
  (let* ((dom (get-dom pic-html-name))
	 (pic-dom (dom-by-tag dom 'li)))
    pic-dom))

(defun pics-parser-1 (dom class)
  (car (cdr (car (dom-by-class dom class)))))

(defun get-pics-info ()
  "得到 pic 的信息"
  (let* ((dom (get-all-pic-dom))
	 (pics-info
	  (let (pics-in)
	    (dolist (element dom pics-in)
	      (push (cons (alist-get 'href (pics-parser-1 element "preview"))
			  (if (pics-parser-1 element "png")
			      ".png"
			    ".jpg"))
		    pics-in))
	    pics-in)))
    pics-info))

(defun get-sub-pics-name (str)
  (nth 3 (split-string str "/+")))


;; 下载

(defun get-all-picture ()
  "使用 emacs 下载图片 (卡死了,难受)"
  (let ((all-info (get-pics-info)))
    (dolist (element all-info)
      (get-picture (get-pics-name (car element)) (cdr element)))))

(defun get-all-pic-urls (&optional filter)
  (let* ((all-info (get-pics-info))
	 (all-urls
	  (let ((urls nil))
	    (dolist (element all-info urls)
	      (setq urls
		    (cons (com-pic-url (get-pics-name (car element)) (cdr element))
			  urls)))
	    urls)))
    all-urls))


;; filter
;; pic-url-filename
(defun pic-get-download-files ()
  (if (file-directory-p pic-dir-filename)
      (let ((files (directory-files pic-dir-filename)))
	files)
    (message "图片文件夹不存在")))

(defun pic-filter-meth (matcher data)
  (let (return-dat)
    (dolist (element data return-dat)
      (setq return-dat
	    (if (string-match-p matcher element)
		(delete element data)
	      data)))
    return-dat))

(defun pic-filter (data)
  "过滤掉下载过的文件"
  (let ((match-dat (cdr (cdr (pic-get-download-files))))
	(return-dat))
    (dolist (element match-dat return-dat)
      (setq return-dat (pic-filter-meth element data)))
    return-dat))



(defun create-urls-to-file (&optional filter)
  (with-current-buffer (find-file pic-urls-cache-filename)
    (kill-region (point-min) (point-max))
    (let ((all-urls (if filter
			(apply filter (list (get-all-pic-urls)))
		      (get-all-pic-urls))))
      (dolist (element all-urls)
	(insert (concat element "\n"))))
    (save-buffer)))

(defun pic-update-html ()
  (interactive)
  (with-current-buffer (find-file pic-html-name)))

(defun pic-update-url ()
  (interactive)
  (create-urls-to-file #'pic-filter))

(provide 'wallhaven)
