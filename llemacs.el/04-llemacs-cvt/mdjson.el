;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 15:41:03
;;; Time-stamp: <2025-01-04 15:41:03 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-cvt/mdjson.el

(defcustom llemacs--cvt-format-script
  (expand-file-name "bin/mdjson" llemacs--path-python-env-sys)
  "Path to JSON/Markdown conversion script."
  :type 'file
  :group 'llemacs)

;; (defun llemacs--cvt-mdjson (md-or-json-path)
;;   "Convert MD/JSON file to JSON/MD using external script."
;;   (interactive
;;    (list (let ((default (and buffer-file-name buffer-file-name)))
;;            (read-file-name "File: " nil default t default))))
;;   (condition-case err
;;       (call-process llemacs--cvt-format-script nil nil nil (expand-file-name md-or-json-path))
;;     (error
;;      (llemacs--logging-write-error-sys (format "Conversion failed: %s" err))
;;      nil)))

(defun llemacs--cvt-mdjson (md-or-json-path)
  "Convert MD/JSON file to JSON/MD using external script."
  (interactive
   (list (let ((default (and buffer-file-name buffer-file-name)))
           (read-file-name "File: " nil default t default))))
  (condition-case err
      (call-process llemacs--cvt-format-script nil nil nil (expand-file-name md-or-json-path))
    (error
     (llemacs--logging-write-error-sys (format "Conversion failed: %s" err))
     0)))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))