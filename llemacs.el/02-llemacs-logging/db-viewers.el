;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 10:54:56
;;; Time-stamp: <2025-01-02 10:54:56 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/db-viewers.el

;; (require '02-llemacs-logging-core-utils)
;; (require '02-llemacs-logging-db-getters)

(defun llemacs--logging-view-project-logs (project-id)
  "View logs for PROJECT-ID in buffer."
  (interactive "sProject ID: ")
  (let* ((logs (llemacs--logging-get-project-logs project-id))
         (buf (get-buffer-create "*LLEMACS Project Logs*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (log logs)
        (insert (format "[%s] Project %s: %s\n"
                        (elt log 0)
                        (elt log 1)
                        (elt log 4)))))
    (llemacs--logging-display-buffer buf)))

(defun llemacs--logging-view-milestone-logs (milestone-id)
  "View logs for MILESTONE-ID in buffer."
  (interactive "sMilestone ID: ")
  (let* ((logs (llemacs--logging-get-milestone-logs milestone-id))
         (buf (get-buffer-create "*LLEMACS Milestone Logs*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (log logs)
        (insert (format "[%s] Milestone %s: %s\n"
                        (elt log 0)
                        (elt log 3)
                        (elt log 5)))))
    (llemacs--logging-display-buffer buf)))

(defun llemacs--logging-view-task-logs (task-id)
  "View logs for TASK-ID in buffer."
  (interactive "sTask ID: ")
  (let* ((logs (llemacs--logging-get-task-logs task-id))
         (buf (get-buffer-create "*LLEMACS Task Logs*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (log logs)
        (let ((timestamp (elt log 0))
              (task-id (elt log 4))
              (state (elt log 5)))
          (insert (format "[%s] Task %s: %s\n"
                          timestamp task-id state)))))
    (llemacs--logging-display-buffer buf)))

(defun llemacs--logging-view-step-logs (step-id)
  "View logs for STEP-ID in buffer."
  (interactive "sStep ID: ")
  (let* ((logs (llemacs--logging-get-step-logs step-id))
         (buf (get-buffer-create "*LLEMACS Step Logs*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (log logs)
        (let ((timestamp (elt log 0))
              (step-id (elt log 4))
              (result (elt log 7)))
          (insert (format "[%s] Step %s: %s\n"
                          timestamp step-id result)))))
    (llemacs--logging-display-buffer buf)))

(defun llemacs--logging-view-project-chain (project-id)
  "View full project chain for PROJECT-ID."
  (interactive "sProject ID: ")
  (let* ((chain (llemacs--logging-get-project-chain project-id))
         (buf (get-buffer-create "*LLEMACS Project Chain*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Project ===\n")
      (dolist (log (alist-get 'project chain))
        (insert (format "[%s] %s: %s\n"
                        (elt log 0)
                        (elt log 1)
                        (elt log 4))))
      (insert "\n=== Milestones ===\n")
      (dolist (log (alist-get 'milestones chain))
        (insert (format "[%s] %s: %s\n"
                        (elt log 0)
                        (elt log 3)
                        (elt log 5))))
      (insert "\n=== Tasks ===\n")
      (dolist (log (alist-get 'tasks chain))
        (insert (format "[%s] %s: %s\n"
                        (elt log 0)
                        (elt log 4)
                        (elt log 5))))
      (insert "\n=== Steps ===\n")
      (dolist (log (alist-get 'steps chain))
        (insert (format "[%s] %s: %s\n"
                        (elt log 0)
                        (elt log 4)
                        (elt log 7)))))
    (llemacs--logging-display-buffer buf)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))