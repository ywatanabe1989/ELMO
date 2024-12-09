;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 04:34:52
;;; Time-stamp: <2024-12-06 04:34:52 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-server.el


(require 'cl-lib)
(require 'ninja-config)
(require 'ninja-sudo)

(defcustom ninja-server-check-interval 1
  "Interval in seconds to check server status."
  :type 'integer
  :group 'ninja-server)

(defun ninja-server-running-p ()
  "Check if NINJA server is running."
  (interactive)
  (ninja--run-sudo-server-script "status"))
;; (ninja-server-running-p)

(defun ninja-kill-server ()
  "Stop NINJA server."
  (interactive)
  (ninja--run-sudo-server-script "stop"))
;; (ninja-kill-server)

(defun ninja-init-server ()
  "Start NINJA server."
  (interactive)
  (ninja--run-sudo-server-script "start"))

(defun ninja-restart-server ()
  "Restart NINJA server."
  (interactive)
  (ninja--run-sudo-server-script "restart"))

(defun ninja-reload-server ()
  "Restart NINJA server."
  (interactive)
  (ninja--run-sudo-server-script "execute '(load-file /home/ninja/.emacs.d/init.el)"))

(defun ninja-ensure-server ()
  "Ensure NINJA server is running."
  (interactive)
  (let ((was-started nil))
    (unless (ninja-server-running-p)
      (ninja-init-server)
      (let ((attempts 0))
        (while (and (not (ninja-server-running-p))
                    (< attempts 5))
          (message "Waiting for server... (%s)" ninja-server-script-output)
          (sleep-for ninja-server-check-interval)
          (cl-incf attempts))
        (setq was-started t)))
    was-started))
;; (ninja-ensure-server)

(defun ninja-init-or-connect ()
  "Initialize server if not running, then connect."
  (interactive)
  (let ((result (ninja-ensure-server)))
    (when (or result (ninja-server-running-p))
      t)))
;; (ninja-init-or-connect)



(provide 'ninja-server)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; (ninja-kill-server)
;; (ninja-server-running-p) ; nil
;; (ninja-init-server) # works
;; (ninja-server-running-p) ; t
;; (ninja-kill-server)
;; (ninja-server-running-p) ; nil
;; (ninja-restart-server)
;; (ninja-server-running-p) ; t
;; (ninja-ensure-server)
;; (ninja-init-or-connect)
;; (ninja-exec-elisp-code "'\(message \"Hello NINJA!!!\"\)'")


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
