;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 05:20:39
;;; Time-stamp: <2024-12-06 05:20:39 (ywatanabe)>
;;; File: ./self-evolving-agent/src/llemacs-sudo.el


(require 'llemacs-config)


(defun llemacs-sudo-get-password ()
  "Get sudo password once and store it."
  (unless llemacs-sudo-password
    (setq llemacs-sudo-password (read-passwd "Sudo password: ")))
  llemacs-sudo-password)

;; (defun llemacs-run-sudo-server-script (command)
;;   (with-temp-buffer
;;     (let* ((process-connection-type nil)
;;            (proc (start-process "llemacs-sudo" (current-buffer)
;;                               "sudo" "-S" "-p" "" llemacs-server-script-path command)))
;;       (process-send-string proc (concat (llemacs-sudo-get-password) "\n"))
;;       (set-process-sentinel proc #'ignore)
;;       (while (process-live-p proc)
;;         (sleep-for 0.1))
;;       (setq llemacs-server-script-output (buffer-string))
;;       (unless (= 0 (process-exit-status proc))
;;         (error "Sudo command failed: %s" llemacs-server-script-output))
;;       (process-exit-status proc))))


(defun llemacs-run-sudo-server-script (command)
  (let ((inhibit-read-only t)
        (process-connection-type t))
    (with-current-buffer (get-buffer-create "*llemacs-output*")
      (erase-buffer)
      (let ((proc (start-process-shell-command
                   "llemacs-sudo" (current-buffer)
                   (format "echo '%s' | sudo -S %s %s >/dev/null 2>&1"
                          (llemacs-sudo-get-password)
                          (expand-file-name llemacs-server-script-path)
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

;; (llemacs-run-sudo-server-script "status")
;; (llemacs-run-sudo-server-script "kill")
;; (llemacs-run-sudo-server-script "init")
;; (llemacs-run-sudo-server-script "status")
;; (llemacs-run-sudo-server-script "execute ls")


(provide 'llemacs-sudo)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
