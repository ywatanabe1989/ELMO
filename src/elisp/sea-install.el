;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 01:23:22
;;; Time-stamp: <2024-12-06 01:23:22 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-install.el


(require 'sea-config)
(require 'cl-lib)
(require 'auth-source)
(require 'sea-verify-installation)
(require 'sea-logging)


(defun sea-setup-sudo ()
  "Setup sudo configuration for SEA."
  (interactive)
  (let ((sudo-file "/etc/sudoers.d/sea-emacs")
        (temp-file (make-temp-file "sea-sudo"))
        (content (format "%s ALL=(%s) NOPASSWD: %s\n"
                        (user-login-name)
                        sea-user
                        sea-emacs-cli)))
    (write-region content nil temp-file)
    (call-process "sudo" nil nil nil
                 "cp" temp-file sudo-file)
    (call-process "sudo" nil nil nil
                 "chown" "root:root" sudo-file)
    (call-process "sudo" nil nil nil
                 "chmod" "440" sudo-file)
    (delete-file temp-file)))

;; (sea-setup-sudo)

;; # /etc/sudoers.d/sea-emacs
;; ywatanabe ALL=(sea) NOPASSWD: /usr/bin/emacsclient


(defun sea--check-dependencies ()
  "Check if required system dependencies are available."
  (let ((required-commands '("git" "sudo" "python3"))
        (missing-commands '()))

    (dolist (cmd required-commands)
      (unless (executable-find cmd)
        (push cmd missing-commands)))

    (when missing-commands
      (error "Missing required commands: %s"
             (string-join missing-commands ", ")))))

;; (defun sea--create-user (username)
;;   "Create a new system user for SEA."
;;   (unless (zerop (shell-command
;;                   (format "id %s >/dev/null 2>&1" username)))
;;     (shell-command
;;      (format "sudo useradd -m -s /bin/bash %s" username))
;;     (shell-command
;;      (format "sudo usermod -aG sudo %s" username))))

(defun sea--setup-workspace ()
  "Initialize SEA workspace with symbolic links."
  (interactive)
  (let* ((user-name (user-login-name))
         (source-dir (directory-file-name sea-user-root-dir))
         (workspace-dir (directory-file-name sea-workspace-dir))
         (target-link (expand-file-name "self-evolving-agent" workspace-dir)))

    ;; Verify user is in sea group
    (unless (member "sea" (split-string (shell-command-to-string
                                       (format "groups %s" user-name))))
      (error "Current user must be in 'sea' group. Run install.sh first"))

    ;; Create base directories
    (dolist (dir (list sea-work-dir
                      sea-workspace-dir
                      sea-backups-dir
                      sea-logs-dir
                      sea-command-logs-dir
                      sea-requests-dir
                      sea-config-dir))
      (unless (file-exists-p dir)
        (make-directory dir t)
        (set-file-modes dir #o700)))

    ;; Touch request files
    (dolist (file (list sea-user-request-file
                       sea-request-file))
      (unless (file-exists-p file)
        (write-region "" nil file)))

    ;; Create symbolic link
    (when (file-exists-p target-link)
      (delete-file target-link))
    (make-symbolic-link source-dir target-link)))

(defun sea--user-exists-p (username)
  "Check if USERNAME exists in the system."
  (zerop (shell-command
          (format "id %s >/dev/null 2>&1" username))))

(defun sea--setup-user (main-user)
  "Set up SEA user and configure permissions for MAIN-USER."
  (unless (sea--user-exists-p main-user)
    (error "User %s does not exist" main-user))

  (unless (sea--user-exists-p "sea")
    (sea--log-message "Creating sea user...")
    (unless (zerop (shell-command "sudo useradd -r -m -d /home/sea sea"))
      (error "Failed to create sea user"))
    (shell-command "sudo chmod 755 /home/sea"))

  (sea--log-message "Configuring groups...")
  (shell-command (format "sudo usermod -aG sea %s" main-user))
  (shell-command (format "sudo usermod -aG %s sea" main-user))
  )

(defun sea--setup-git-config ()
  "Configure git settings for SEA user."
  (sea--log-message "Setting up git configuration for sea user...")

  (let ((git-commands
         '("git config --global user.name \"sea-bot\""
           "git config --global user.email \"sea-bot@example.com\""
           "git config --global core.editor \"gedit\""
           "git config --global init.defaultBranch \"main\"")))
    (dolist (cmd git-commands)
      (shell-command (format "sudo -u sea %s" cmd))))

  (let ((gitignore (expand-file-name ".gitignore_global" sea-config-dir)))
    (with-temp-file gitignore
      (insert "*~\n.DS_Store\n.env\n*.log\n"))
    (shell-command (format "sudo -u sea git config --global core.excludesfile %s" gitignore))
    (shell-command (format "sudo chmod 600 %s" gitignore))
    (sea--log-message "Git configuration completed")))

;; (defun sea--setup-github-token ()
;;   "Set up GitHub token interactively."
;;   (sea--log-message "Setting up GitHub token...")

;;   (when (file-exists-p sea-github-token-file)
;;     (let* ((default-token (with-temp-buffer
;;                            (insert-file-contents sea-github-token-file)
;;                            (buffer-string)))
;;            (masked-token (concat (substring default-token 0 4)
;;                                "..."
;;                                (substring default-token -4))))
;;       (let ((input (read-string
;;                    (format "Enter GitHub Token (Enter for %s, s to skip): "
;;                           masked-token))))
;;         (cond ((string-empty-p input)
;;                (sea--log-message "Keeping existing token")
;;                (cl-return-from sea--setup-github-token t))
;;               ((string= input "s")
;;                (sea--log-message "Skipping token setup")
;;                (cl-return-from sea--setup-github-token t))))))

;;   (let ((token (read-string "Enter GitHub Personal Access Token (s to skip): ")))
;;     (when (string= token "s")
;;       (sea--log-message "Skipping token setup")
;;       (cl-return-from sea--setup-github-token t))

;;     (when (< (length token) 40)
;;       (sea--log-message "Error: Invalid token length")
;;       (cl-return-from sea--setup-github-token nil))

;;     (with-temp-file sea-github-token-file
;;       (insert token))
;;     (shell-command (format "sudo chmod 600 %s" sea-github-token-file))
;;     (sea--log-message "GitHub token saved")))

(cl-defun sea--setup-github-token ()
  "Set up GitHub token interactively."
  (sea--log-message "Setting up GitHub token...")

  (when (file-exists-p sea-github-token-file)
    (let* ((default-token (with-temp-buffer
                           (insert-file-contents sea-github-token-file)
                           (buffer-string)))
           (masked-token (concat (substring default-token 0 4)
                               "..."
                               (substring default-token -4))))
      (let ((input (read-string
                   (format "Enter GitHub Token (Enter for %s, s to skip): "
                          masked-token))))
        (cond ((string-empty-p input)
               (sea--log-message "Keeping existing token")
               (cl-return-from sea--setup-github-token t))
              ((string= input "s")
               (sea--log-message "Skipping token setup")
               (cl-return-from sea--setup-github-token t))))))

  (let ((token (read-string "Enter GitHub Personal Access Token (s to skip): ")))
    (when (string= token "s")
      (sea--log-message "Skipping token setup")
      (cl-return-from sea--setup-github-token t))

    (when (< (length token) 40)
      (sea--log-message "Error: Invalid token length")
      (cl-return-from sea--setup-github-token nil))

    (with-temp-file sea-github-token-file
      (insert token))
    (shell-command (format "sudo chmod 600 %s" sea-github-token-file))
    (sea--log-message "GitHub token saved")))



(defun sea--install-dependencies ()
  "Install required system packages and Emacs packages."
  (sea--log-message "Installing dependencies...")

  ;; System packages
  (let ((packages '("python3" "curl" "wget")))
    (dolist (pkg packages)
      (unless (zerop (shell-command (format "which %s >/dev/null 2>&1" pkg)))
        (sea--log-message (format "Installing %s..." pkg))
        (let ((result (shell-command (format "sudo apt-get install -y %s" pkg))))
          (unless (zerop result)
            (display-warning 'sea (format "Failed to install %s" pkg) :error))))))

  ;; Python packages
  (let* ((default-directory sea-workspace-dir)
         (venv-dir (expand-file-name ".env" sea-workspace-dir)))
    ;; Create and activate virtual environment
    (unless (file-exists-p venv-dir)
      (shell-command "python3 -m venv .env"))

    (let ((commands
           `(,(format "bash -c 'source %s/bin/activate && pip install --upgrade pip'" venv-dir)
             ,(format "bash -c 'source %s/bin/activate && pip install -r %s/requirements.txt'"
                     venv-dir sea-user-root-dir))))
      (dolist (cmd commands)
        (let ((result (shell-command cmd)))
          (unless (zerop result)
            (display-warning 'sea
                           (format "Failed to execute command: %s" cmd)
                           :error))))))

  ;; Emacs packages
  (condition-case err
      (progn
        (require 'package)
        (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
        (package-initialize)
        (package-refresh-contents)

        (dolist (pkg '(markdown-mode request async))
          (unless (package-installed-p pkg)
            (condition-case nil
                (package-install pkg)
              (error
               (display-warning 'sea
                              (format "Failed to install package: %s" pkg)
                              :error))))))
    (error
     (display-warning 'sea
                     (format "Error during Emacs package setup: %s" (error-message-string err))
                     :error))))

;; (defun sea--install-dependencies ()
;;   "Install required system packages and Emacs packages."
;;   (sea--log-message "Installing dependencies...")

;;   ;; System packages
;;   (let ((packages '("python3" "curl" "wget")))
;;     (dolist (pkg packages)
;;       (unless (zerop (shell-command (format "which %s >/dev/null 2>&1" pkg)))
;;         (sea--log-message (format "Installing %s..." pkg))
;;         (shell-command (format "sudo apt-get install -y %s" pkg)))))

;;   ;; Python packages
;;   (shell-command "cd sea-workspace-dir")
;;   (shell-command "python -m pip install -U pip")
;;   (shell-command "python -m venv .env")
;;   (shell-command "source .env/bin/activate")
;;   (shell-command "python -m pip install -U pip")
;;   (shell-command (concat "python -m pip install -r " sea-user-root-dir "requirements.txt"))

;;   ;; Emacs packages
;;   (require 'package)
;;   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;   (package-initialize)
;;   (package-refresh-contents)

;;   (dolist (pkg '(markdown-mode request async))
;;     (unless (package-installed-p pkg)
;;       (package-install pkg))))

(defun sea--setup-permissions ()
  "Set correct permissions for SEA directories and files."
  (sea--log-message "Setting up permissions...")

  ;; Directory permissions
  (mapc (lambda (dir)
          (set-file-modes dir #o755)
          (shell-command (format "sudo chown -R sea:sea %s" dir)))
        (list sea-work-dir
              sea-workspace-dir
              sea-source-dir
              sea-backups-dir
              sea-logs-dir
              sea-requests-dir
              sea-config-dir))

  ;; Special file permissions
  (when (file-exists-p sea-github-token-file)
    (set-file-modes sea-github-token-file #o600))

  (dolist (file (directory-files-recursively sea-logs-dir ".*\\.log$"))
    (set-file-modes file #o644)))

(defun sea--backup-existing-files ()
  "Backup existing SEA files if they exist."
  (when (file-exists-p sea-work-dir)
    (let ((backup-dir (format "%s/sea-backup-%s"
                             temporary-file-directory
                             (format-time-string "%Y%m%d-%H%M%S"))))
      (make-directory backup-dir t)
      (copy-directory sea-work-dir backup-dir t t t)
      (sea--log-message
       (format "Existing files backed up to %s" backup-dir)))))

;; (defun sea--create-directories ()
;;   "Create all necessary directories for SEA."
;;   (mapc (lambda (dir)
;;           (unless (file-exists-p dir)
;;             (make-directory dir t)))
;;         (list sea-work-dir
;;               sea-workspace-dir
;;               sea-source-dir
;;               sea-backups-dir
;;               sea-logs-dir
;;               sea-requests-dir
;;               sea-config-dir)))

(defun sea--create-initial-files ()
  "Create initial files and templates."
  (sea--log-message "Creating initial files...")

  ;; Create user request template
  (with-temp-file sea-user-request-file
    (insert "# Improvement Request\n\n"
            "## Description\n\n"
            "## Expected Outcome\n\n"
            "## Additional Notes\n"))

  ;; Create SEA request template
  (with-temp-file sea-request-file
    (insert "# Self-Improvement Proposal\n\n"
            "## Current Limitation\n\n"
            "## Proposed Changes\n\n"
            "## Implementation Plan\n\n"
            "## Testing Strategy\n"))

  ;; Initialize history log
  (unless (file-exists-p sea-history-file)
    (with-temp-file sea-history-file
      (insert (format-time-string "# SEA History Log\nInitialized on %Y-%m-%d %H:%M:%S\n\n")))))

;; (defun sea--verify-installation ()
;;   "Verify that all components are properly installed and configured."
;;   (sea--log-message "Verifying installation...")

;;   (let ((checks
;;          `((,sea-work-dir "Main working directory")
;;            (,sea-workspace-dir "Workspace directory")
;;            (,sea-source-dir "Source directory")
;;            (,sea-logs-dir "Logs directory")
;;            (,sea-config-dir "Config directory")
;;            (,sea-github-token-file "GitHub token file")
;;            (,sea-user-request-file "User request template")
;;            (,sea-request-file "SEA request template")
;;            (,sea-history-file "History log"))))

;;     (cl-loop for (path desc) in checks
;;              do (unless (file-exists-p path)
;;                   (error "Missing %s at %s" desc path))))

;;   (sea--log-message "Installation verified successfully"))

(defun sea--setup-environment ()
  "Set up SEA environment variables and shell configuration."
  (sea--log-message "Setting up environment...")

  (let ((env-file (expand-file-name ".env" sea-config-dir)))
    (with-temp-file env-file
      (insert (format "SEA_ROOT=%s\n" sea-work-dir)
              (format "SEA_WORKSPACE=%s\n" sea-workspace-dir)
              (format "SEA_SOURCE=%s\n" sea-source-dir)
              (format "SEA_LOGS=%s\n" sea-logs-dir)
              (format "SEA_CONFIG=%s\n" sea-config-dir)))
    (shell-command (format "sudo chmod 644 %s" env-file))))

(defun sea-install (&optional main-user)
  "Install SEA system with MAIN-USER as primary user.
If MAIN-USER is nil, use current user."
  (interactive)

  (let ((main-user (or main-user (user-login-name))))
    (condition-case err
        (progn
          (sea--log-message "Starting SEA installation...")

          ;; Core setup
          (sea--setup-user main-user)
          ;; Directories and files
          (sea--setup-workspace)

          ;; Pre-installation checks
          (sea--check-dependencies)

          ;; Git/GitHub
          (sea--setup-git-config)
          (sea--setup-github-token)

          ;; Repository and dependencies
          (sea--install-dependencies)

          ;; Configuration and files
          (sea--setup-permissions)
          (sea--create-initial-files)
          (sea--setup-environment)

          ;; Final verification
          (sea-verify-installation)

          (sea-setup-sudo)

          (sea--log-message "SEA installation completed successfully!")
          t)

      (error
       (sea--log-message (format "Installation failed: %s" (error-message-string err)))
       nil))))

(provide 'sea-install)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; (require 'sea)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
