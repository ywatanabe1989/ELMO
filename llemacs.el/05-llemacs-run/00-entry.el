;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-05 05:00:13
;;; Time-stamp: <2025-01-05 05:00:13 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/00-entry.el

;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 23:16:00
;;; Time-stamp: <2025-01-04 23:16:00 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/00-entry.el

(defun llemacs--load-run-components ()
  "Load run component files."
  (let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (load (expand-file-name "run-elisp.el" dir))
    (load (expand-file-name "run-prompt.el" dir))
    (load (expand-file-name "run-project.el" dir))
    ;; (load (expand-file-name "helpers.el" dir))
    ))

(llemacs--load-run-components)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))