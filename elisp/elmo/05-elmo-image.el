;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-24 18:02:38
;;; Time-stamp: <2024-12-24 18:02:38 (ywatanabe)>
;;; File: /home/ywatanabe/.emacs.d/lisp/elmo/elisp/elmo/elmo-image.el

(defun elmo-display-image (file)
  (let ((image-type 'jpeg))
    (with-current-buffer (get-buffer-create "*elmo-image*")
      (erase-buffer)
      (let ((inhibit-read-only t))
        (insert-image-file file)
        (image-mode)
        (image-transform-fit-to-window))
      (display-buffer (current-buffer)))))

(defun elmo-save-image (data filename)
  (with-temp-file filename
    (set-buffer-multibyte nil)
    (insert data))
  (sleep-for 1)
  (elmo-display-image filename))

(provide '05-elmo-image)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))