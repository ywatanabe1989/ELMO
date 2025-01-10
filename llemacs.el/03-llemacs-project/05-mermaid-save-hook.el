;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 07:36:04
;;; Timestamp: <2025-01-11 07:36:04>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-project/05-mermaid-save-hook.el

;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 19:33:23
;;; Timestamp: <2025-01-09 19:33:23>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-project/05-mermaid-save-hook.el


(cl-defun llemacs--mermaid-compile (&optional filename buffer)
  "Generate PNG, SVG, and GIF from Mermaid content."
  (interactive)
  (when (and (not filename) (not (bound-and-true-p mermaid-mode)))
    (cl-return-from llemacs--mermaid-compile nil))

  (let ((mmdc-path (llemacs--path-find-bin "mmdc" "mermaid-cli"))
        (convert-path (llemacs--path-find-bin "convert" "magick")))
    (let* ((temp-buffer (generate-new-buffer " *temp*"))
           (contents (if filename
                         (with-temp-buffer
                           (insert-file-contents filename)
                           (buffer-string))
                       (if buffer
                           (with-current-buffer buffer
                             (buffer-string))
                         (buffer-string))))
           (input-file filename))

      (unless input-file
        (llemacs--logging-write-error-pj "Buffer is not visiting a file"))

      (let ((output-buffer (get-buffer-create "*mermaid-output*")))
        (with-current-buffer temp-buffer
          (insert contents)
          (llemacs--logging-write-info-pj
           (format "Starting mermaid compilation for %s" input-file))

          (let* ((base-name (file-name-sans-extension input-file))
                 (png-file (concat base-name ".png"))
                 (svg-file (concat base-name ".svg"))
                 (gif-file (concat base-name ".gif")))

            (condition-case err
                (progn
                  (call-process-region (point-min) (point-max) mmdc-path nil output-buffer t
                                       "-i" input-file
                                       "-o" png-file
                                       "--backgroundColor" "transparent"
                                       "-s 3")

                  (call-process-region (point-min) (point-max) mmdc-path nil output-buffer t
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
              (error
               (llemacs--logging-write-error-pj
                (format "Failed to compile mermaid: %s" (error-message-string err))))))
          (kill-buffer temp-buffer))))))

(defun llemacs--pj-mermaid-compile ()
  "Compiles project management mermaid diagram into image files.
Processes /project_management/project_management.mmd in current project
('llemacs--path-pj-pm-mmd'), generating corresponding
PNG, SVG, and GIF files."
  (llemacs--ensure-original-goals)
  (llemacs--mermaid-compile llemacs--path-pj-pm-mmd))

(use-package mermaid-mode
  :ensure t
  :custom
  (setq mermaid-output-format ".png"))

(use-package ob-mermaid
  :ensure t
  :custom
  (ob-mermaid-cli-path "/usr/local/bin/mmdc")
  (ob-mermaid-extra-args "--backgroundColor transparent -f svg"))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
