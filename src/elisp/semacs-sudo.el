;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 05:20:39
;;; Time-stamp: <2024-12-06 05:20:39 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-sudo.el


(require 'semacs-config)


(defun semacs--sudo-get-password ()
  "Get sudo password once and store it."
  (unless semacs--sudo-password
    (setq semacs--sudo-password (read-passwd "Sudo password: ")))
  semacs--sudo-password)

;; (defun semacs--run-sudo-server-script (command)
;;   (with-temp-buffer
;;     (let* ((process-connection-type nil)
;;            (proc (start-process "semacs-sudo" (current-buffer)
;;                               "sudo" "-S" "-p" "" semacs-server-script-path command)))
;;       (process-send-string proc (concat (semacs--sudo-get-password) "\n"))
;;       (set-process-sentinel proc #'ignore)
;;       (while (process-live-p proc)
;;         (sleep-for 0.1))
;;       (setq semacs-server-script-output (buffer-string))
;;       (unless (= 0 (process-exit-status proc))
;;         (error "Sudo command failed: %s" semacs-server-script-output))
;;       (process-exit-status proc))))


(defun semacs--run-sudo-server-script (command)
  (let ((inhibit-read-only t)
        (process-connection-type t))
    (with-current-buffer (get-buffer-create "*semacs-output*")
      (erase-buffer)
      (let ((proc (start-process-shell-command
                   "semacs-sudo" (current-buffer)
                   (format "echo '%s' | sudo -S %s %s >/dev/null 2>&1"
                          (semacs--sudo-get-password)
                          (expand-file-name semacs-server-script-path)
                          command))))
        (set-process-filter proc
          (lambda (proc output)
            (with-current-buffer (process-buffer proc)
              (goto-char (point-max))
              (insert output))))
        (set-process-sentinel proc
          (lambda (proc event)
            (when (string= event "finished\n")
              (message "Command completed"))))
        (display-buffer (current-buffer))
        (while (process-live-p proc)
          (sleep-for 0.1))
        (process-exit-status proc)))))

;; (semacs--run-sudo-server-script "status")
;; (semacs--run-sudo-server-script "kill")
;; (semacs--run-sudo-server-script "init")
;; (semacs--run-sudo-server-script "status")
;; (semacs--run-sudo-server-script "execute ls")


(provide 'semacs-sudo)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
