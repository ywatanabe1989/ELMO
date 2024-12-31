;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 23:34:30
;;; Time-stamp: <2024-12-31 23:34:30 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/llemacs.el

(defun llemacs--load-components ()
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "01-llemacs-base.el" dir))
    (load (expand-file-name "02-llemacs-logging.el" dir))
    (load (expand-file-name "03-llemacs-llm.el" dir))
    (load (expand-file-name "04-llemacs-cvt.el" dir))

    ))

(llemacs--load-components)

;; (require '01-llemacs-config)
;; (require '02-llemacs-logging)
;; (require '02-llemacs-utils)
;; (require '03-llemacs-llm)
;; (require '03-llemacs-prompt)
;; (require '03-llemacs-prompt-recipes)
;; (require '04-llemacs-exec)
;; (require '04-llemacs-json-md)
;; (require '04-llemacs-lang2elisp)
;; (require '05-llemacs-context)
;; (require '05-llemacs-project)
;; (require '05-llemacs-step)
;; (require '06-llemacs-compress)
;; (require '06-llemacs-run)
;; (require '06-llemacs-search)

(provide 'llemacs)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))