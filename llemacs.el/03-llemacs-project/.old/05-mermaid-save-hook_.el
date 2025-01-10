;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 19:33:23
;;; Timestamp: <2025-01-09 19:33:23>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-project/05-mermaid-save-hook.el


(defun llemacs--mermaid-compile (&optional filename buffer)
  "Generate PNG, SVG, and GIF from Mermaid content."
  (interactive)
  (when (and (not filename) (not (bound-and-true-p mermaid-mode)))
    (message "Not in mermaid-mode. Skipping compilation.")
    (cl-return-from llemacs--mermaid-compile nil))
  (let ((mmdc-path (llemacs--path-find-bin "mmdc" "mermaid-cli"))
        (convert-path (llemacs--path-find-bin "convert" "magick")))
    (let* ((buf (cond
                 (filename (let ((b (find-file-noselect filename)))
                             (unless (derived-mode-p 'mermaid-mode)
                               (mermaid-mode))
                             b))
                 (buffer buffer)
                 (t (current-buffer))))
           (input-file (buffer-file-name buf)))
      (unless input-file
        (llemacs--logging-write-error-pj "Buffer is not visiting a file"))
      (let ((output-buffer (get-buffer-create "*mermaid-output*")))
        (with-current-buffer buf
          (llemacs--logging-write-info-pj
           (format "Starting mermaid compilation for %s" input-file))
          (let* ((base-name (file-name-sans-extension input-file))
                 (png-file (concat base-name ".png"))
                 (svg-file (concat base-name ".svg"))
                 (gif-file (concat base-name ".gif")))
            (condition-case err
                (progn
                  (call-process mmdc-path nil output-buffer t
                                "-i" input-file
                                "-o" png-file
                                "--backgroundColor" "transparent")
                  (call-process mmdc-path nil output-buffer t
                                "-i" input-file
                                "-o" svg-file
                                "--backgroundColor" "transparent"
                                "-f" "svg")
                  (call-process convert-path nil output-buffer t
                                png-file
                                gif-file)
                  (if (and (file-exists-p png-file)
                           (file-exists-p svg-file)
                           (file-exists-p gif-file))
                      (llemacs--logging-write-success-pj
                       (format "Created %s.{png,svg,gif}" base-name))
                    (progn
                      (llemacs--logging-write-error-pj
                       (format "Some files were not generated: PNG:%s SVG:%s GIF:%s"
                               (if (file-exists-p png-file) "yes" "no")
                               (if (file-exists-p svg-file) "yes" "no")
                               (if (file-exists-p gif-file) "yes" "no")))
                      (llemacs--logging-write-error-pj
                       (format "Command output:\n%s"
                               (with-current-buffer output-buffer
                                 (buffer-string)))))))
              (llemacs--logging-write-error-pj
               (llemacs--logging-write-error-pj
                (format "Failed to compile mermaid: %s" (llemacs--logging-write-error-pj-message-string err)))))))))))

(defun llemacs--pj-mermaid-compile ()
  "Compiles project management mermaid diagram into image files.
  Processes /project_management/project_management.mmd in current project
  (‘llemacs--path-pj-pm-mmd’), generating corresponding
  PNG, SVG, and GIF files."
  (llemacs--mermaid-compile llemacs--path-pj-pm-mmd))

;; (llemacs--pj-mermaid-compile)


(defun llemacs--mermaid-after-save-hook ()
  (llemacs--mermaid-compile))

(use-package mermaid-mode
  :ensure t
  :custom
  (setq mermaid-output-format ".png")
  :hook
  (after-save . llemacs--mermaid-after-save-hook))

(use-package ob-mermaid
  :ensure t
  :custom
  (ob-mermaid-cli-path "/usr/local/bin/mmdc")
  (ob-mermaid-extra-args "--backgroundColor transparent -f svg"))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
