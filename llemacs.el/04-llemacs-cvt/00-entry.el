;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 04:01:33
;;; Time-stamp: <2025-01-02 04:01:33 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/04-llemacs-cvt/00-entry.el

(defun llemacs--load-cvt-components ()
  "Load converter component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "t2t-compress.el" dir))
    (load (expand-file-name "mdjson.el" dir))
    (load (expand-file-name "lang2elisp.el" dir))))

(llemacs--load-cvt-components)

(provide '04-llemacs-cvt)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))