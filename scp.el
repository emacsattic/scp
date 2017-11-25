;;; scp.el --- Use the SCP command to transfer files with the remote server
;;
;; Copyright (C) 2017 zg
;;
;; Author: zg <13853850881@163.com>
;; URL: https://github.com/tszg/emacs-scp
;; Package-Version: 0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Keywords: convenience, scp
;;
;;; Commentary:
;; (require 'scp)
;; Linux USES the SCP command and the sshpass command
;; The PSCP command line tool is used under Windows
;;
;;; Code:

(require 'cl-lib)

(defconst scp-tools
  (if (memq system-type '(windows-nt ms-dos))
      "pscp"
    "scp")
  "Set scp tool.")

(defcustom scp-buffer-name "*scp*"
  "Result Buffer name."
  :type 'string)

(define-derived-mode scp-mode org-mode "Scp"
  "Major mode for viewing Scp result."
  (read-only-mode 1)
  (define-key scp-mode-map "q" 'quit-window))

(defun scp-locals-init()
  "Get local variables"
  (setq enable-local-variables :all enable-local-eval t)
  (hack-dir-local-variables))

(defun scp-exist-dir-locals-file()
  "Determine whether the dir-local -file file exists"
  (file-exists-p (concat (locate-dominating-file default-directory dir-locals-file) dir-locals-file)))

;; (scp-locals-init)

(defun scp-show-in-buffer()
  (with-current-buffer (get-buffer-create scp-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (scp-mode)
      (goto-char (point-min)))
    (switch-to-buffer-other-window scp-buffer-name)))

(defun scp-remote-file-path(local_path)
  "String replacement to get the remote file path"
  (replace-regexp-in-string (locate-dominating-file default-directory dir-locals-file)
			    (concat (scp-get-alist 'remote_path) "/")
			    local_path))

(cl-defun scp-get-alist(key &optional (alist file-local-variables-alist))
  "Gets the value of the Association List"
  (cdr (assoc key alist)))

(defun scp-cmd(status &optional local_path)
  "Stitching scp command"
  (scp-locals-init)
  (let* ((host (scp-get-alist 'host))
	 (user (scp-get-alist 'user))
	 (port (scp-get-alist 'port))
	 (pw (scp-get-alist 'password))
	 (cmd (concat (unless (memq system-type '(windows-nt ms-dos))
			(format "sshpass -p %s " pw))
		      scp-tools  (unless (eq local_path buffer-file-name) " -r")
		      (concat " -P " port " "
			      (when (memq system-type '(windows-nt ms-dos))
				(format "-pw %s " pw)))))
	 (remote_path (concat (scp-remote-file-path local_path)
			      (if (and (not (eq local_path buffer-file-name)) (string-equal status "get"))
				  "*")))
	 (cmd_list (if (string= status "put")
		       (format "%s %s@%s:%s" local_path user host remote_path)
		     (format "%s@%s:%s %s" user host remote_path local_path))))
    (concat cmd cmd_list)))

(cl-defun scp(status &optional (directory buffer-file-name))
  "scp operation"
  ;; (message (concat "scp " status "......"))
  (cond ((not (executable-find scp-tools))
	 (message  (format "Please install the %s" scp-tools)))
	((not (scp-exist-dir-locals-file))
	 (message "Please set the configuration file"))
	((and (not (memq system-type '(windows-nt ms-dos))) (not (executable-find "sshpass")))
	 (message "Please install the sshpass"))
	(t (start-process-shell-command "scp" (scp-show-in-buffer) (scp-cmd status directory)))))

(defun scp-get()
  "download"
  (interactive)
  (scp "get"))

(defun scp-put()
  "Upload"
  (interactive)
  (scp "put"))

(defun scp-get-directory(root)
  "Download the folder"
  (interactive "DLocal directory: ")
  (scp "get" root))

(defun scp-put-directory(root)
  "Upload folder"
  (interactive "DLocal directory: ")
  (scp "put" root))

(provide 'scp)

;;; scp.el ends here