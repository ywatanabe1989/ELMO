;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 10:54:42
;;; Time-stamp: <2025-01-02 10:54:42 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/04-llemacs-cvt/00-entry.el

(defun llemacs--load-cvt-components ()
  "Load converter component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "t2t-compress.el" dir))
    (load (expand-file-name "mdjson.el" dir))
    (load (expand-file-name "lang2elisp.el" dir))))

(llemacs--load-cvt-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))