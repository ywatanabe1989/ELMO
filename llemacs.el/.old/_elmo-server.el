;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 04:34:52
;;; Time-stamp: <2024-12-06 04:34:52 (ywatanabe)>
;;; File: ./self-evolving-agent/src/llemacs-server.el


(require 'cl-lib)
(require 'llemacs-config)
(require 'llemacs-sudo)

(defcustom llemacs-server-check-interval 1
  "Interval in seconds to check server status."
  :type 'integer
  :group 'llemacs-server)

(defun llemacs-server-running-p ()
  "Check if ELMO server is running."
  (interactive)
  (llemacs-run-sudo-server-script "status"))
;; (llemacs-server-running-p)

(defun llemacs-kill-server ()
  "Stop ELMO server."
  (interactive)
  (llemacs-run-sudo-server-script "stop"))
;; (llemacs-kill-server)

(defun llemacs-init-server ()
  "Start ELMO server."
  (interactive)
  (llemacs-run-sudo-server-script "start"))

(defun llemacs-restart-server ()
  "Restart ELMO server."
  (interactive)
  (llemacs-run-sudo-server-script "restart"))

(defun llemacs-reload-server ()
  "Restart ELMO server."
  (interactive)
  (llemacs-run-sudo-server-script "execute '(load-file /home/llemacs/.emacs.d/init.el)"))

(defun llemacs-ensure-server ()
  "Ensure ELMO server is running."
  (interactive)
  (let ((was-started nil))
    (unless (llemacs-server-running-p)
      (llemacs-init-server)
      (let ((attempts 0))
        (while (and (not (llemacs-server-running-p))
                    (< attempts 5))
          (message "Waiting for server... (%s)" llemacs-server-script-output)
          (sleep-for llemacs-server-check-interval)
          (cl-incf attempts))
        (setq was-started t)))
    was-started))
;; (llemacs-ensure-server)

(defun llemacs-init-or-connect ()
  "Initialize server if not running, then connect."
  (interactive)
  (let ((result (llemacs-ensure-server)))
    (when (or result (llemacs-server-running-p))
      t)))
;; (llemacs-init-or-connect)



(provide 'llemacs-server)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; (llemacs-kill-server)
;; (llemacs-server-running-p) ; nil
;; (llemacs-init-server) # works
;; (llemacs-server-running-p) ; t
;; (llemacs-kill-server)
;; (llemacs-server-running-p) ; nil
;; (llemacs-restart-server)
;; (llemacs-server-running-p) ; t
;; (llemacs-ensure-server)
;; (llemacs-init-or-connect)
;; (llemacs-exec-elisp-code "'\(message \"Hello ELMO!!!\"\)'")


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
