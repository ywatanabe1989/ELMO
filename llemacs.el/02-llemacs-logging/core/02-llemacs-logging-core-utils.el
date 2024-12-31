;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 17:17:20
;;; Time-stamp: <2024-12-31 17:17:20 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging/core/utils.el

(require '01-llemacs-config)

(defun llemacs--logging-display-buffer (buffer)
  "Display BUFFER in a dedicated window."
  (let ((window (split-window-below -10)))
    (set-window-buffer window buffer)
    (with-current-buffer buffer
      (goto-char (point-max))
      (recenter -1))
    (select-window window)))

(defun llemacs--log-get-caller-info ()
  "Get caller's file and line info."
  (let* ((frames (backtrace-frames))
         (frame (nth 4 frames)))
    (when frame
      (format "%s:%s"
              (or load-file-name buffer-file-name "unknown")
              (line-number-at-pos)))))

(defun llemacs--logging-format-message (level message)
  "Format log message with LEVEL and MESSAGE."
  (format "[%s][%s][%s] %s"
          (format-time-string "%Y-%m-%d %H:%M:%S")
          (upcase (symbol-name level))
          (llemacs--log-get-caller-info)
          message))

(defun llemacs--logging-timestamp ()
  "Get current timestamp in ISO 8601 format."
  (format-time-string "%Y-%m-%dT%H:%M:%S"))

(defun llemacs--logging-project-parent ()
  "Get current project's parent ID or empty string."
  (or (and (boundp 'llemacs--current-project-parent)
           llemacs--current-project-parent)
      ""))

(defun llemacs--logging-task-parent ()
  "Get current task's parent ID or empty string."
  (or (and (boundp 'llemacs--current-task-parent)
           llemacs--current-task-parent)
      ""))

(provide '02-llemacs-logging-core-utils)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))