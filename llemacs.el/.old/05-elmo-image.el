;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-24 18:02:38
;;; Time-stamp: <2024-12-24 18:02:38 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/llemacs/elisp/llemacs/llemacs-image.el

(defun llemacs-display-image (file)
  (let ((image-type 'jpeg))
    (with-current-buffer (get-buffer-create "*llemacs-image*")
      (erase-buffer)
      (let ((inhibit-read-only t))
        (insert-image-file file)
        (image-mode)
        (image-transform-fit-to-window))
      (display-buffer (current-buffer)))))

(defun llemacs-save-image (data filename)
  (with-temp-file filename
    (set-buffer-multibyte nil)
    (insert data))
  (sleep-for 1)
  (llemacs-display-image filename))

(provide '05-llemacs-image)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))