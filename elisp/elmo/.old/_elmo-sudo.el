;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 05:20:39
;;; Time-stamp: <2024-12-06 05:20:39 (ywatanabe)>
;;; File: ./self-evolving-agent/src/elmo-sudo.el


(require 'elmo-config)


(defun elmo-sudo-get-password ()
  "Get sudo password once and store it."
  (unless elmo-sudo-password
    (setq elmo-sudo-password (read-passwd "Sudo password: ")))
  elmo-sudo-password)

;; (defun elmo-run-sudo-server-script (command)
;;   (with-temp-buffer
;;     (let* ((process-connection-type nil)
;;            (proc (start-process "elmo-sudo" (current-buffer)
;;                               "sudo" "-S" "-p" "" elmo-server-script-path command)))
;;       (process-send-string proc (concat (elmo-sudo-get-password) "\n"))
;;       (set-process-sentinel proc #'ignore)
;;       (while (process-live-p proc)
;;         (sleep-for 0.1))
;;       (setq elmo-server-script-output (buffer-string))
;;       (unless (= 0 (process-exit-status proc))
;;         (error "Sudo command failed: %s" elmo-server-script-output))
;;       (process-exit-status proc))))


(defun elmo-run-sudo-server-script (command)
  (let ((inhibit-read-only t)
        (process-connection-type t))
    (with-current-buffer (get-buffer-create "*elmo-output*")
      (erase-buffer)
      (let ((proc (start-process-shell-command
                   "elmo-sudo" (current-buffer)
                   (format "echo '%s' | sudo -S %s %s >/dev/null 2>&1"
                          (elmo-sudo-get-password)
                          (expand-file-name elmo-server-script-path)
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

;; (elmo-run-sudo-server-script "status")
;; (elmo-run-sudo-server-script "kill")
;; (elmo-run-sudo-server-script "init")
;; (elmo-run-sudo-server-script "status")
;; (elmo-run-sudo-server-script "execute ls")


(provide 'elmo-sudo)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
