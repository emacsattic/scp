;;; init-sftp.el A SFTP implementation based on SCP command

;; Copyright (C) 2016-2017 zg

;; Author: zg <13853850881@163.com>
;; URL: https://github.com/tszg/emacs-sftp
;; Package-Version: 20171122.1720
;; Package-X-Original-Version: 0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, sftp

;;; Commentary:

;;; Code:

(require 'cl)

(defconst sftp-tools
  (if (string-equal system-type "windows-nt")
      "pscp"
    "scp")
  "Set sftp tool")

(defcustom sftp-buffer-name "*sftp*"
  "Result Buffer name."
  :type 'string)

(define-derived-mode sftp-mode org-mode "Sftp Desc"
  "Major mode for viewing sftp result."
  (read-only-mode 1)
  (define-key sftp-mode-map "q" 'quit-window))

(defun sftp-locals-init()
  "Get local variables"
  (setq enable-local-variables :all enable-local-eval t)
  (hack-dir-local-variables))

(defun sftp-exist-dir-locals-file()
  "Determine whether the dir-local -file file exists"
  (file-exists-p (concat (locate-dominating-file default-directory dir-locals-file) dir-locals-file)))

(sftp-locals-init)

(defun sftp-show-in-buffer(msg)
  (with-current-buffer (get-buffer-create sftp-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (sftp-mode)
      (insert msg)
      (goto-char (point-min))
      (set (make-local-variable 'zg-sftp-current-buffer-msg) msg))
    (switch-to-buffer-other-window sftp-buffer-name)))

(defun sftp-remote-file-path(local_path)
  "String replacement to get the remote file path"
  (replace-regexp-in-string (locate-dominating-file default-directory dir-locals-file)
			    (concat (zg/get-alist 'remote_path) "/")
			    local_path))

(cl-defun sftp-get-alist(key &optional (alist file-local-variables-alist))
  "Gets the value of the Association List"
  (cdr (assoc key alist)))

(defun sftp-cmd(status &optional local_path)
  "Stitching sftp command"
  (sftp-locals-init)
  (let* ((host (sftp-get-alist 'host))
	 (user (sftp-get-alist 'user))
	 (port (sftp-get-alist 'port))
	 (pw (sftp-get-alist 'password))
	 (cmd (concat sftp-tools  (unless (eq local_path buffer-file-name) " -r")
		      (format " -P %s -pw %s " port pw)))
	 (remote_path (concat (sftp-remote-file-path local_path)
			      (if (and (not (eq local_path buffer-file-name)) (string-equal status "get"))
				  "*")))
	 (cmd_list (if (string= status "put")
		       (format "%s %s@%s:%s" local_path user host remote_path)
		     (format "%s@%s:%s %s" user host remote_path local_path))))
    (concat cmd cmd_list)))

(cl-defun sftp(status &optional (directory buffer-file-name))
  "sftp operation"
  (message (concat "sftp " status "......"))
  (cond ((not (executable-find sftp-tools))
	 (message  (format "Please install the %s" sftp-tools)))
	((not (sftp-exist-dir-locals-file))
	 (message "Please set the configuration file"))
	(t (sftp-show-in-buffer (shell-command-to-string (sftp-cmd status directory))))))

(defun sftp-get()
  "download"
  (interactive)
  (sftp "get"))

(defun sftp-put()
  "Upload"
  (interactive)
  (sftp "put"))

(defun sftp-get-directory(root)
  "Download the folder"
  (interactive "DLocal directory: ")
  (sftp "get" root))

(defun sftp-put-directory(root)
  "Upload folder"
  (interactive "DLocal directory: ")
  (sftp "put" root))

(global-set-key (kbd "C-c d") 'sftp-get)
(global-set-key (kbd "C-c u") 'sftp-put)


(provide 'sftp)
