;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-31 16:59:36
;;; Time-stamp: <2024-12-31 16:59:36 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/llemacs.el/02-llemacs-logging-initializers-file.el

;; Just placeholder for consistency

(provide '02-llemacs-logging-file)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))