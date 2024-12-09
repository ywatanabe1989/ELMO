;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 00:00:21
;;; Time-stamp: <2024-12-06 00:00:21 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-image.el


(defun sea--display-image (file)
  (let ((image-type 'jpeg))
    (with-current-buffer (get-buffer-create "*sea-image*")
      (erase-buffer)
      (let ((inhibit-read-only t))
        (insert-image-file file)
        (image-mode)
        (image-transform-fit-to-window))
      (display-buffer (current-buffer)))))

(defun sea--save-image (data filename)
  (with-temp-file filename
    (set-buffer-multibyte nil)
    (insert data))
  (sleep-for 1)
  (sea--display-image filename))


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
