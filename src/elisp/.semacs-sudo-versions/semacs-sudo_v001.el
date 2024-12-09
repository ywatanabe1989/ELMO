;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-05 23:57:48
;;; Time-stamp: <2024-12-05 23:57:48 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-sudo.el


(require 'semacs-config)


(defun semacs--sudo-get-password ()
  "Get sudo password once and store it."
  (unless semacs--sudo-password
    (setq semacs--sudo-password (read-passwd "Sudo password: ")))
  semacs--sudo-password)

(defun semacs--run-sudo-server-script (command)
  (with-temp-buffer
    (let* ((process-connection-type nil)
           (proc (start-process "semacs-sudo" (current-buffer)
                              "sudo" "-S" "-p" "" semacs-server-script-path command)))
      (process-send-string proc (concat (semacs--sudo-get-password) "\n"))
      (set-process-sentinel proc #'ignore)
      (while (process-live-p proc)
        (sleep-for 0.1))
      (setq semacs-server-script-output (buffer-string))
      (unless (= 0 (process-exit-status proc))
        (error "Sudo command failed: %s" semacs-server-script-output))
      (process-exit-status proc))))

;; (defun semacs--run-sudo-server-script (command)
;;   (with-temp-buffer
;;     (let ((proc (start-process "semacs-sudo" (current-buffer)
;;                               "sudo" "-S" semacs-server-script-path command)))
;;       (process-send-string proc (concat (semacs--sudo-get-password) "\n"))
;;       (while (process-live-p proc)
;;         (sleep-for 0.1))
;;       (setq semacs-server-script-output (buffer-string))
;;       (process-exit-status proc))))

;; (defun semacs--run-sudo-server-script (command)
;;   "Run server command with sudo."
;;   (with-temp-buffer
;;     (let ((proc (start-process "semacs-sudo" (current-buffer)
;;                               "sudo" "-S" semacs-server-script-path command)))
;;       (process-send-string proc (concat (semacs--sudo-get-password) "\n"))
;;       (while (process-live-p proc)
;;         (sleep-for 0.1))
;;       (setq semacs-server-script-output (buffer-string))
;;       (process-exit-status proc))))


;; (defun semacs--run-sudo-server-script (command)
;;   "Run server command with sudo."
;;   (with-temp-buffer
;;     (let* ((process-environment
;;             (append
;;              (list
;;               (format "SEMACS_USER=%s" semacs-user)
;;               (format "SEMACS_SOCKET_NAME=semacs"))
;;              process-environment))
;;            (passwd (semacs--sudo-get-password)))
;;       (insert passwd "\n")
;;       (let ((exit-code (call-process-region (point-min) (point-max)
;;                                           "sudo" t t nil
;;                                           "-S" semacs-server-script-path command)))
;;         (setq semacs-server-script-output (buffer-string))
;;         (if (string-match-p "Server is running" semacs-server-script-output)
;;             t
;;           nil)))))

(provide 'semacs-sudo)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
