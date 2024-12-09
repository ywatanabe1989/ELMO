;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 04:34:52
;;; Time-stamp: <2024-12-06 04:34:52 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-server.el


(require 'cl-lib)
(require 'semacs-config)
(require 'semacs-sudo)

(defcustom semacs-server-check-interval 1
  "Interval in seconds to check server status."
  :type 'integer
  :group 'semacs-server)

(defun semacs-server-running-p ()
  "Check if SEMACS server is running."
  (interactive)
  (semacs--run-sudo-server-script "status"))
;; (semacs-server-running-p)

(defun semacs-kill-server ()
  "Stop SEMACS server."
  (interactive)
  (semacs--run-sudo-server-script "stop"))
;; (semacs-kill-server)

(defun semacs-init-server ()
  "Start SEMACS server."
  (interactive)
  (semacs--run-sudo-server-script "start"))

(defun semacs-restart-server ()
  "Restart SEMACS server."
  (interactive)
  (semacs--run-sudo-server-script "restart"))

(defun semacs-reload-server ()
  "Restart SEMACS server."
  (interactive)
  (semacs--run-sudo-server-script "execute '(load-file /home/semacs/.emacs.d/init.el)"))

(defun semacs-ensure-server ()
  "Ensure SEMACS server is running."
  (interactive)
  (let ((was-started nil))
    (unless (semacs-server-running-p)
      (semacs-init-server)
      (let ((attempts 0))
        (while (and (not (semacs-server-running-p))
                    (< attempts 5))
          (message "Waiting for server... (%s)" semacs-server-script-output)
          (sleep-for semacs-server-check-interval)
          (cl-incf attempts))
        (setq was-started t)))
    was-started))
;; (semacs-ensure-server)

(defun semacs-init-or-connect ()
  "Initialize server if not running, then connect."
  (interactive)
  (let ((result (semacs-ensure-server)))
    (when (or result (semacs-server-running-p))
      t)))
;; (semacs-init-or-connect)



(provide 'semacs-server)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; (semacs-kill-server)
;; (semacs-server-running-p) ; nil
;; (semacs-init-server) # works
;; (semacs-server-running-p) ; t
;; (semacs-kill-server)
;; (semacs-server-running-p) ; nil
;; (semacs-restart-server)
;; (semacs-server-running-p) ; t
;; (semacs-ensure-server)
;; (semacs-init-or-connect)
;; (semacs-exec-elisp-code "'\(message \"Hello SEMACS!!!\"\)'")


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
