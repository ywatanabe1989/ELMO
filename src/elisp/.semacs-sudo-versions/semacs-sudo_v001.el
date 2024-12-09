;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-05 23:57:48
;;; Time-stamp: <2024-12-05 23:57:48 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-sudo.el


(require 'ninja-config)


(defun ninja--sudo-get-password ()
  "Get sudo password once and store it."
  (unless ninja--sudo-password
    (setq ninja--sudo-password (read-passwd "Sudo password: ")))
  ninja--sudo-password)

(defun ninja--run-sudo-server-script (command)
  (with-temp-buffer
    (let* ((process-connection-type nil)
           (proc (start-process "ninja-sudo" (current-buffer)
                              "sudo" "-S" "-p" "" ninja-server-script-path command)))
      (process-send-string proc (concat (ninja--sudo-get-password) "\n"))
      (set-process-sentinel proc #'ignore)
      (while (process-live-p proc)
        (sleep-for 0.1))
      (setq ninja-server-script-output (buffer-string))
      (unless (= 0 (process-exit-status proc))
        (error "Sudo command failed: %s" ninja-server-script-output))
      (process-exit-status proc))))

;; (defun ninja--run-sudo-server-script (command)
;;   (with-temp-buffer
;;     (let ((proc (start-process "ninja-sudo" (current-buffer)
;;                               "sudo" "-S" ninja-server-script-path command)))
;;       (process-send-string proc (concat (ninja--sudo-get-password) "\n"))
;;       (while (process-live-p proc)
;;         (sleep-for 0.1))
;;       (setq ninja-server-script-output (buffer-string))
;;       (process-exit-status proc))))

;; (defun ninja--run-sudo-server-script (command)
;;   "Run server command with sudo."
;;   (with-temp-buffer
;;     (let ((proc (start-process "ninja-sudo" (current-buffer)
;;                               "sudo" "-S" ninja-server-script-path command)))
;;       (process-send-string proc (concat (ninja--sudo-get-password) "\n"))
;;       (while (process-live-p proc)
;;         (sleep-for 0.1))
;;       (setq ninja-server-script-output (buffer-string))
;;       (process-exit-status proc))))


;; (defun ninja--run-sudo-server-script (command)
;;   "Run server command with sudo."
;;   (with-temp-buffer
;;     (let* ((process-environment
;;             (append
;;              (list
;;               (format "NINJA_USER=%s" ninja-user)
;;               (format "NINJA_SOCKET_NAME=ninja"))
;;              process-environment))
;;            (passwd (ninja--sudo-get-password)))
;;       (insert passwd "\n")
;;       (let ((exit-code (call-process-region (point-min) (point-max)
;;                                           "sudo" t t nil
;;                                           "-S" ninja-server-script-path command)))
;;         (setq ninja-server-script-output (buffer-string))
;;         (if (string-match-p "Server is running" ninja-server-script-output)
;;             t
;;           nil)))))

(provide 'ninja-sudo)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
