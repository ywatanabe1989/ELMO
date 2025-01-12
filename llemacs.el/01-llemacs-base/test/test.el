;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-08 05:22:21
;;; Timestamp: <2025-01-08 05:22:21>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/test/test.el
;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

(require 'ert)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
(package-install 'emacrsql)

(load-file (expand-file-name "../../../llemacs.el" (or load-file-name buffer-file-name)))

;; Setup test environment
;; (llemacs--pj-lock-force-release (llemacs--pj-get-cur-pj))
(llemacs--pj-set-cur-pj "000-test" t)


(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (let ((tail (mapcar (lambda (f) (expand-file-name f test-dir))
                      '("test-llemacs-base.el"
                        "test-llemacs-buffers.el"
                        "test-llemacs-loaders.el"
                        "test-llemacs-locks.el"
                        "test-llemacs-paths.el"
                        "test-llemacs-logging.el"))))
    (while tail
      (let ((file (car tail)))
        (load file)
        (setq tail (cdr tail))))))

;; Run tests
(ert-run-tests-batch-and-exit)

;; emacs -Q -batch -l /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-base/test/test.el



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
