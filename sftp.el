;;; sftp.el --- Use the SCP command to transfer files with the remote server
;;
;; Copyleft (C) 2017 zg
;;
;; Author: zg <13853850881@163.com>
;; URL: https://github.com/tszg/emacs-sftp
;; Package-Version: 0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: convenience, sftp
;;
;;; Commentary:
;; (require 'sftp)
;; Linux USES the SCP command and the sshpass command
;; The PSCP command line tool is used under Windows
;;
;;; Code:

(require 'cl-lib)

(defconst sftp-tools
  (if (memq system-type '(windows-nt ms-dos))
      "pscp"
    "scp")
  "Set sftp tool.")

(defcustom sftp-buffer-name "*sftp*"
  "Result Buffer name."
  :type 'string)

(define-derived-mode sftp-mode org-mode "Sftp"
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

;; (sftp-locals-init)

(defun sftp-show-in-buffer(msg)
  (with-current-buffer (get-buffer-create sftp-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (sftp-mode)
      (insert msg)
      (goto-char (point-min))
      (set (make-local-variable 'sftp-current-buffer-msg) msg))
    (switch-to-buffer-other-window sftp-buffer-name)))

(defun sftp-remote-file-path(local_path)
  "String replacement to get the remote file path"
  (replace-regexp-in-string (locate-dominating-file default-directory dir-locals-file)
			    (concat (sftp-get-alist 'remote_path) "/")
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
	 (cmd (concat (unless (memq system-type '(windows-nt ms-dos))
			(format "sshpass -p %s " pw))
		      sftp-tools  (unless (eq local_path buffer-file-name) " -r")
		      (concat " -P " port " "
			      (when (memq system-type '(windows-nt ms-dos))
				(format "-pw %s " pw)))))
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
	((and (not (memq system-type '(windows-nt ms-dos))) (not (executable-find "sshpass")))
	 (message "Please install the sshpass"))
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

;; (global-set-key (kbd "C-c d") 'sftp-get)
;; (global-set-key (kbd "C-c u") 'sftp-put)

(provide 'sftp)

;;; sftp.el ends here
