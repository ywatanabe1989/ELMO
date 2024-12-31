;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 23:34:05
;;; Time-stamp: <2024-12-31 23:34:05 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/04-llemacs-cvt.el

(defun llemacs--load-cvt-components ()
  "Load converter component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "04-llemacs-cvt-compress.el" dir))
    (load (expand-file-name "04-llemacs-cvt-json-and-md.el" dir))
    (load (expand-file-name "04-llemacs-cvt-json-and-md.sh" dir))
    (load (expand-file-name "04-llemacs-cvt-lang2elisp.el" dir))))

(llemacs--load-cvt-components)

(provide '04-llemacs-cvt)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))