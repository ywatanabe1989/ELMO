;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 04:34:52
;;; Time-stamp: <2024-12-06 04:34:52 (ywatanabe)>
;;; File: ./self-evolving-agent/src/elmo-server.el


(require 'cl-lib)
(require 'elmo-config)
(require 'elmo-sudo)

(defcustom elmo-server-check-interval 1
  "Interval in seconds to check server status."
  :type 'integer
  :group 'elmo-server)

(defun elmo-server-running-p ()
  "Check if ELMO server is running."
  (interactive)
  (elmo-run-sudo-server-script "status"))
;; (elmo-server-running-p)

(defun elmo-kill-server ()
  "Stop ELMO server."
  (interactive)
  (elmo-run-sudo-server-script "stop"))
;; (elmo-kill-server)

(defun elmo-init-server ()
  "Start ELMO server."
  (interactive)
  (elmo-run-sudo-server-script "start"))

(defun elmo-restart-server ()
  "Restart ELMO server."
  (interactive)
  (elmo-run-sudo-server-script "restart"))

(defun elmo-reload-server ()
  "Restart ELMO server."
  (interactive)
  (elmo-run-sudo-server-script "execute '(load-file /home/elmo/.emacs.d/init.el)"))

(defun elmo-ensure-server ()
  "Ensure ELMO server is running."
  (interactive)
  (let ((was-started nil))
    (unless (elmo-server-running-p)
      (elmo-init-server)
      (let ((attempts 0))
        (while (and (not (elmo-server-running-p))
                    (< attempts 5))
          (message "Waiting for server... (%s)" elmo-server-script-output)
          (sleep-for elmo-server-check-interval)
          (cl-incf attempts))
        (setq was-started t)))
    was-started))
;; (elmo-ensure-server)

(defun elmo-init-or-connect ()
  "Initialize server if not running, then connect."
  (interactive)
  (let ((result (elmo-ensure-server)))
    (when (or result (elmo-server-running-p))
      t)))
;; (elmo-init-or-connect)



(provide 'elmo-server)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; (elmo-kill-server)
;; (elmo-server-running-p) ; nil
;; (elmo-init-server) # works
;; (elmo-server-running-p) ; t
;; (elmo-kill-server)
;; (elmo-server-running-p) ; nil
;; (elmo-restart-server)
;; (elmo-server-running-p) ; t
;; (elmo-ensure-server)
;; (elmo-init-or-connect)
;; (elmo-exec-elisp-code "'\(message \"Hello ELMO!!!\"\)'")


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
