;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2025-01-04 13:09:52
;;; Time-stamp: <2025-01-04 13:09:52 (ywatanabe)>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/test/test.el

(require 'ert)

(load-file (expand-file-name "~/.emacs.d/init.el"))
(load-file (expand-file-name "../../../llemacs.el" (or load-file-name buffer-file-name)))

;; Setup test environment
(llemacs--pj-lock-force-release "000-test")
(llemacs--pj-set-cur-pj "000-test")

;; Load test files
(dolist (file '("test-llemacs-logging.el"
                "test-llemacs-logging-getters.el"))
  (load (expand-file-name file)))


;; Run tests
(ert-run-tests-batch-and-exit)

;; emacs -Q -batch -l /home/ywatanabe/proj/llemacs/llemacs.el/02-llemacs-logging/test/test.el


;; After loading the test files in Emacs:

;; 1. Run all tests:
;; ```elisp
;; M-x ert RET t RET
;; ```

;; 2. Run specific test:
;; ```elisp
;; M-x ert RET test-llemacs-buffer-naming RET
;; ```

;; 3. Run tests matching pattern:
;; ```elisp
;; M-x ert RET "^test-llemacs-buffer" RET
;; ```

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))