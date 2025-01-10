;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 04:14:32
;;; Timestamp: <2025-01-09 04:14:32>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/test.el

;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-09 03:39:52
;;; Timestamp: <2025-01-09 03:39:52>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/test.el

(require 'llemacs-logging)

(message "from here")

(let ((llemacs--cur-pj "085-Epilepsy-prediction-project"))
  (defun llemacs--test-logging ()
    (llemacs--logging-write-error-pj "Test error message"))
  (llemacs--test-logging))
