;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 05:20:39
;;; Time-stamp: <2024-12-06 05:20:39 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-sudo.el


(require 'ninja-config)


(defun ninja--sudo-get-password ()
  "Get sudo password once and store it."
  (unless ninja--sudo-password
    (setq ninja--sudo-password (read-passwd "Sudo password: ")))
  ninja--sudo-password)

;; (defun ninja--run-sudo-server-script (command)
;;   (with-temp-buffer
;;     (let* ((process-connection-type nil)
;;            (proc (start-process "ninja-sudo" (current-buffer)
;;                               "sudo" "-S" "-p" "" ninja-server-script-path command)))
;;       (process-send-string proc (concat (ninja--sudo-get-password) "\n"))
;;       (set-process-sentinel proc #'ignore)
;;       (while (process-live-p proc)
;;         (sleep-for 0.1))
;;       (setq ninja-server-script-output (buffer-string))
;;       (unless (= 0 (process-exit-status proc))
;;         (error "Sudo command failed: %s" ninja-server-script-output))
;;       (process-exit-status proc))))


(defun ninja--run-sudo-server-script (command)
  (let ((inhibit-read-only t)
        (process-connection-type t))
    (with-current-buffer (get-buffer-create "*ninja-output*")
      (erase-buffer)
      (let ((proc (start-process-shell-command
                   "ninja-sudo" (current-buffer)
                   (format "echo '%s' | sudo -S %s %s >/dev/null 2>&1"
                          (ninja--sudo-get-password)
                          (expand-file-name ninja-server-script-path)
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

;; (ninja--run-sudo-server-script "status")
;; (ninja--run-sudo-server-script "kill")
;; (ninja--run-sudo-server-script "init")
;; (ninja--run-sudo-server-script "status")
;; (ninja--run-sudo-server-script "execute ls")


(provide 'ninja-sudo)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
