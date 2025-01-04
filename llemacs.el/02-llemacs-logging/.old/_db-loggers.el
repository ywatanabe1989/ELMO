;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-02 10:55:07
;;; Time-stamp: <2025-01-02 10:55:07 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/db-loggers.el

;; (require '02-llemacs-logging-core-utils)
;; (require '02-llemacs-logging-core-db)
;; (require '02-llemacs-logging-db-initializers)

(defmacro llemacs--logging-with-project (project-id title desc &rest body)
  "Log project with PROJECT-ID, TITLE, DESC and execute BODY."
  `(progn
     (unless llemacs--logging-db-connection
       (llemacs--logging-db-init-if))
     (emacsql llemacs--logging-db-connection
              [:insert :into projects
                       :values $v1]
              (vector (llemacs--logging-timestamp)
                      ,project-id
                      (llemacs--logging-project-parent)
                      ,title
                      ,desc))
     ,@body))

(defmacro llemacs--logging-with-milestone (milestone-id project-id title desc &rest body)
  "Log milestone with MILESTONE-ID under PROJECT-ID with TITLE, DESC and execute BODY."
  `(progn
     (unless llemacs--logging-db-connection
       (llemacs--logging-db-init-if))
     (emacsql llemacs--logging-db-connection
              [:insert :into milestones
                       :values $v1]
              (vector (llemacs--logging-timestamp)
                      ,project-id
                      (llemacs--logging-project-parent)
                      ,milestone-id
                      ,title
                      ,desc))
     ,@body))

(defmacro llemacs--logging-with-task (task-id project-id state &rest body)
  "Log task with TASK-ID under PROJECT-ID with STATE and execute BODY."
  `(progn
     (unless llemacs--logging-db-connection
       (llemacs--logging-db-init-if))
     (emacsql llemacs--logging-db-connection
              [:insert :into tasks
                       :values $v1]
              (vector (llemacs--logging-timestamp)
                      ,project-id
                      (llemacs--logging-project-parent)
                      (llemacs--logging-task-parent)
                      ,task-id
                      ,state))
     ,@body))

(defmacro llemacs--logging-with-step (step-id project-id input expected result &rest body)
  "Log step with STEP-ID under PROJECT-ID with INPUT, EXPECTED, RESULT and execute BODY."
  `(progn
     (unless llemacs--logging-db-connection
       (llemacs--logging-db-init-if))
     (emacsql llemacs--logging-db-connection
              [:insert :into steps
                       :values $v1]
              (vector (llemacs--logging-timestamp)
                      ,project-id
                      (llemacs--logging-project-parent)
                      (llemacs--logging-task-parent)
                      ,step-id
                      ,input
                      ,expected
                      ,result))
     ,@body))

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))