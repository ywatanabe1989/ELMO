;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 08:46:13
;;; Timestamp: <2025-01-11 08:46:13>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/01-llemacs-path/07-pj-symlink.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symlink
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun llemacs--path-pj-update-symlink-linux (pj-id symlink-linux)
  "Update Linux symlink for PJ-ID."
  (message "Linux symlink debug - PJ-ID: %s, Symlink path: %s" pj-id symlink-linux)
  (when (file-exists-p symlink-linux)
    (message "Removing existing Linux symlink")
    (set-file-modes symlink-linux #o777)
    (delete-file symlink-linux t))
  (condition-case err
      (make-symbolic-link pj-id symlink-linux t)
    (error (message "Failed to create Linux symlink: %s" err))))

(defun llemacs--check-wsl ()
  "Check if running in WSL environment."
  (and (eq system-type 'gnu/linux)
       (string-match "Microsoft"
                     (with-temp-buffer
                       (insert-file-contents "/proc/version")
                       (buffer-string)))))

(defun llemacs--path-pj-update-symlink-windows (pj-id symlink-win target-path)
  "Update Windows symlink at Documents for PJ-ID."
  (let ((temp-ps1 (make-temp-file "create_symlink" nil ".ps1")))
    (with-temp-file temp-ps1
      (insert (format "
$targetPath = \"%s\"
$linkPath = \"%s\"
if (Test-Path $linkPath) {
    Remove-Item $linkPath -Force -Confirm:$false -Recurse
}
Start-Process powershell -Verb RunAs -ArgumentList \"-Command New-Item -ItemType SymbolicLink -Path `\"$linkPath`\" -Target `\"$targetPath`\" -Force -Confirm:`$false\"
" target-path llemacs--path-pj-win-shortcut)))
    (shell-command (format "powershell.exe -ExecutionPolicy Bypass -File %s" temp-ps1))
    (delete-file temp-ps1)))

(defun llemacs--path-pj-update-symlink (&optional pj-id)
  "Update symlinks for PJ-ID in both Linux and Windows environments."
  (let* ((pj-id (or pj-id llemacs--cur-pj))
         (pj-dir (expand-file-name pj-id llemacs--path-projects))
         (symlink-linux (expand-file-name "current-project" llemacs--path-projects))
         (symlink-win (concat symlink-linux "-win"))
         (target-path (concat "\\\\wsl.localhost\\Ubuntu" pj-dir)))
    (llemacs--path-pj-update-symlink-linux pj-id symlink-linux)
    (llemacs--path-pj-update-symlink-windows pj-id symlink-win target-path)))

;; (llemacs--path-pj-update-symlink "103-epilepsty-seizure-detection")
