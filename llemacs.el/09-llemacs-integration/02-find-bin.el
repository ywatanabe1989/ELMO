;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 08:39:49
;;; Timestamp: <2025-01-11 08:39:49>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/09-llemacs-integration/02-find-bin.el

;; (defun llemacs--path-find-bin (name &rest alternatives)
;;   "Find executable NAME or its ALTERNATIVES and return path.
;; If none found, signal an error."
;;   (llemacs--logging-write-debug-pj
;;    (format "Searching for executable: %s or %s" name alternatives))
;;   (let ((path (or (executable-find name)
;;                   (cl-loop for alt in alternatives
;;                            thereis (executable-find alt)))))
;;     (if path
;;         (progn
;;           (llemacs--logging-write-success-pj
;;            (format "Found executable at: %s" path))
;;           path)
;;       (let ((err-msg (format "Executable not found: %s or %s"
;;                              name alternatives)))
;;         (llemacs--logging-write-error-pj err-msg)))))

(defun llemacs--path-find-bin (name &rest alternatives)
  "Find executable NAME or its ALTERNATIVES and return path.
If none found, signal an error."
  (if (string= name "python")
      llemacs--path-pj-python
    (llemacs--logging-write-info-pj
     (format "Searching for executable: %s or %s" name alternatives))
    (let ((path (or (executable-find name)
                    (cl-loop for alt in alternatives
                             thereis (executable-find alt)))))
      (if path
          (progn
            (llemacs--logging-write-success-pj
             (format "Found executable at: %s" path))
            path)
        (let ((err-msg (format "Executable not found: %s or %s"
                               name alternatives)))
          (llemacs--logging-write-error-pj err-msg))))))

;; (llemacs--path-find-bin "python")
