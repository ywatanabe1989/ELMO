;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-03 00:44:54
;;; Time-stamp: <2025-01-03 00:44:54 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/llemacs.el

(defun llemacs--load-components ()
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
         (components '("01-llemacs-base"
                       "02-llemacs-logging"
                       "03-llemacs-llm"
                       "04-llemacs-cvt"
                       "05-llemacs-run"
                       "06-llemacs-pm")))
    (dolist (component components)
      ;; (add-to-list 'load-path (expand-file-name component dir))
      (load (expand-file-name (concat component "/00-entry.el") dir)))))

;; Initilize global logging files
;; (llemacs--logging-db-init-if)
(llemacs--logging-system-files-init-if)

(llemacs--load-components)

(provide 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))