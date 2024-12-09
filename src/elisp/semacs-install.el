;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 01:23:22
;;; Time-stamp: <2024-12-06 01:23:22 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-install.el


(require 'semacs-config)
(require 'cl-lib)
(require 'auth-source)
(require 'semacs-verify-installation)
(require 'semacs-logging)


(defun semacs-setup-sudo ()
  "Setup sudo configuration for SEMACS."
  (interactive)
  (let ((sudo-file "/etc/sudoers.d/semacs-emacs")
        (temp-file (make-temp-file "semacs-sudo"))
        (content (format "%s ALL=(%s) NOPASSWD: %s\n"
                        (user-login-name)
                        semacs-user
                        semacs-emacs-cli)))
    (write-region content nil temp-file)
    (call-process "sudo" nil nil nil
                 "cp" temp-file sudo-file)
    (call-process "sudo" nil nil nil
                 "chown" "root:root" sudo-file)
    (call-process "sudo" nil nil nil
                 "chmod" "440" sudo-file)
    (delete-file temp-file)))

;; (semacs-setup-sudo)

;; # /etc/sudoers.d/semacs-emacs
;; ywatanabe ALL=(semacs) NOPASSWD: /usr/bin/emacsclient


(defun semacs--check-dependencies ()
  "Check if required system dependencies are available."
  (let ((required-commands '("git" "sudo" "python3"))
        (missing-commands '()))

    (dolist (cmd required-commands)
      (unless (executable-find cmd)
        (push cmd missing-commands)))

    (when missing-commands
      (error "Missing required commands: %s"
             (string-join missing-commands ", ")))))

;; (defun semacs--create-user (username)
;;   "Create a new system user for SEMACS."
;;   (unless (zerop (shell-command
;;                   (format "id %s >/dev/null 2>&1" username)))
;;     (shell-command
;;      (format "sudo useradd -m -s /bin/bash %s" username))
;;     (shell-command
;;      (format "sudo usermod -aG sudo %s" username))))

(defun semacs--setup-workspace ()
  "Initialize SEMACS workspace with symbolic links."
  (interactive)
  (let* ((user-name (user-login-name))
         (source-dir (directory-file-name semacs-user-root-dir))
         (workspace-dir (directory-file-name semacs-workspace-dir))
         (target-link (expand-file-name "self-evolving-agent" workspace-dir)))

    ;; Verify user is in semacs group
    (unless (member "semacs" (split-string (shell-command-to-string
                                       (format "groups %s" user-name))))
      (error "Current user must be in 'semacs' group. Run install.sh first"))

    ;; Create base directories
    (dolist (dir (list semacs-work-dir
                      semacs-workspace-dir
                      semacs-backups-dir
                      semacs-logs-dir
                      semacs-command-logs-dir
                      semacs-requests-dir
                      semacs-config-dir))
      (unless (file-exists-p dir)
        (make-directory dir t)
        (set-file-modes dir #o700)))

    ;; Touch request files
    (dolist (file (list semacs-user-request-file
                       semacs-request-file))
      (unless (file-exists-p file)
        (write-region "" nil file)))

    ;; Create symbolic link
    (when (file-exists-p target-link)
      (delete-file target-link))
    (make-symbolic-link source-dir target-link)))

(defun semacs--user-exists-p (username)
  "Check if USERNAME exists in the system."
  (zerop (shell-command
          (format "id %s >/dev/null 2>&1" username))))

(defun semacs--setup-user (main-user)
  "Set up SEMACS user and configure permissions for MAIN-USER."
  (unless (semacs--user-exists-p main-user)
    (error "User %s does not exist" main-user))

  (unless (semacs--user-exists-p "semacs")
    (semacs--log-message "Creating semacs user...")
    (unless (zerop (shell-command "sudo useradd -r -m -d /home/semacs semacs"))
      (error "Failed to create semacs user"))
    (shell-command "sudo chmod 755 /home/semacs"))

  (semacs--log-message "Configuring groups...")
  (shell-command (format "sudo usermod -aG semacs %s" main-user))
  (shell-command (format "sudo usermod -aG %s semacs" main-user))
  )

(defun semacs--setup-git-config ()
  "Configure git settings for SEMACS user."
  (semacs--log-message "Setting up git configuration for semacs user...")

  (let ((git-commands
         '("git config --global user.name \"semacs-bot\""
           "git config --global user.email \"semacs-bot@example.com\""
           "git config --global core.editor \"gedit\""
           "git config --global init.defaultBranch \"main\"")))
    (dolist (cmd git-commands)
      (shell-command (format "sudo -u semacs %s" cmd))))

  (let ((gitignore (expand-file-name ".gitignore_global" semacs-config-dir)))
    (with-temp-file gitignore
      (insert "*~\n.DS_Store\n.env\n*.log\n"))
    (shell-command (format "sudo -u semacs git config --global core.excludesfile %s" gitignore))
    (shell-command (format "sudo chmod 600 %s" gitignore))
    (semacs--log-message "Git configuration completed")))

;; (defun semacs--setup-github-token ()
;;   "Set up GitHub token interactively."
;;   (semacs--log-message "Setting up GitHub token...")

;;   (when (file-exists-p semacs-github-token-file)
;;     (let* ((default-token (with-temp-buffer
;;                            (insert-file-contents semacs-github-token-file)
;;                            (buffer-string)))
;;            (masked-token (concat (substring default-token 0 4)
;;                                "..."
;;                                (substring default-token -4))))
;;       (let ((input (read-string
;;                    (format "Enter GitHub Token (Enter for %s, s to skip): "
;;                           masked-token))))
;;         (cond ((string-empty-p input)
;;                (semacs--log-message "Keeping existing token")
;;                (cl-return-from semacs--setup-github-token t))
;;               ((string= input "s")
;;                (semacs--log-message "Skipping token setup")
;;                (cl-return-from semacs--setup-github-token t))))))

;;   (let ((token (read-string "Enter GitHub Personal Access Token (s to skip): ")))
;;     (when (string= token "s")
;;       (semacs--log-message "Skipping token setup")
;;       (cl-return-from semacs--setup-github-token t))

;;     (when (< (length token) 40)
;;       (semacs--log-message "Error: Invalid token length")
;;       (cl-return-from semacs--setup-github-token nil))

;;     (with-temp-file semacs-github-token-file
;;       (insert token))
;;     (shell-command (format "sudo chmod 600 %s" semacs-github-token-file))
;;     (semacs--log-message "GitHub token saved")))

(cl-defun semacs--setup-github-token ()
  "Set up GitHub token interactively."
  (semacs--log-message "Setting up GitHub token...")

  (when (file-exists-p semacs-github-token-file)
    (let* ((default-token (with-temp-buffer
                           (insert-file-contents semacs-github-token-file)
                           (buffer-string)))
           (masked-token (concat (substring default-token 0 4)
                               "..."
                               (substring default-token -4))))
      (let ((input (read-string
                   (format "Enter GitHub Token (Enter for %s, s to skip): "
                          masked-token))))
        (cond ((string-empty-p input)
               (semacs--log-message "Keeping existing token")
               (cl-return-from semacs--setup-github-token t))
              ((string= input "s")
               (semacs--log-message "Skipping token setup")
               (cl-return-from semacs--setup-github-token t))))))

  (let ((token (read-string "Enter GitHub Personal Access Token (s to skip): ")))
    (when (string= token "s")
      (semacs--log-message "Skipping token setup")
      (cl-return-from semacs--setup-github-token t))

    (when (< (length token) 40)
      (semacs--log-message "Error: Invalid token length")
      (cl-return-from semacs--setup-github-token nil))

    (with-temp-file semacs-github-token-file
      (insert token))
    (shell-command (format "sudo chmod 600 %s" semacs-github-token-file))
    (semacs--log-message "GitHub token saved")))



(defun semacs--install-dependencies ()
  "Install required system packages and Emacs packages."
  (semacs--log-message "Installing dependencies...")

  ;; System packages
  (let ((packages '("python3" "curl" "wget")))
    (dolist (pkg packages)
      (unless (zerop (shell-command (format "which %s >/dev/null 2>&1" pkg)))
        (semacs--log-message (format "Installing %s..." pkg))
        (let ((result (shell-command (format "sudo apt-get install -y %s" pkg))))
          (unless (zerop result)
            (display-warning 'semacs (format "Failed to install %s" pkg) :error))))))

  ;; Python packages
  (let* ((default-directory semacs-workspace-dir)
         (venv-dir (expand-file-name ".env" semacs-workspace-dir)))
    ;; Create and activate virtual environment
    (unless (file-exists-p venv-dir)
      (shell-command "python3 -m venv .env"))

    (let ((commands
           `(,(format "bash -c 'source %s/bin/activate && pip install --upgrade pip'" venv-dir)
             ,(format "bash -c 'source %s/bin/activate && pip install -r %s/requirements.txt'"
                     venv-dir semacs-user-root-dir))))
      (dolist (cmd commands)
        (let ((result (shell-command cmd)))
          (unless (zerop result)
            (display-warning 'semacs
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
               (display-warning 'semacs
                              (format "Failed to install package: %s" pkg)
                              :error))))))
    (error
     (display-warning 'semacs
                     (format "Error during Emacs package setup: %s" (error-message-string err))
                     :error))))

;; (defun semacs--install-dependencies ()
;;   "Install required system packages and Emacs packages."
;;   (semacs--log-message "Installing dependencies...")

;;   ;; System packages
;;   (let ((packages '("python3" "curl" "wget")))
;;     (dolist (pkg packages)
;;       (unless (zerop (shell-command (format "which %s >/dev/null 2>&1" pkg)))
;;         (semacs--log-message (format "Installing %s..." pkg))
;;         (shell-command (format "sudo apt-get install -y %s" pkg)))))

;;   ;; Python packages
;;   (shell-command "cd semacs-workspace-dir")
;;   (shell-command "python -m pip install -U pip")
;;   (shell-command "python -m venv .env")
;;   (shell-command "source .env/bin/activate")
;;   (shell-command "python -m pip install -U pip")
;;   (shell-command (concat "python -m pip install -r " semacs-user-root-dir "requirements.txt"))

;;   ;; Emacs packages
;;   (require 'package)
;;   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;   (package-initialize)
;;   (package-refresh-contents)

;;   (dolist (pkg '(markdown-mode request async))
;;     (unless (package-installed-p pkg)
;;       (package-install pkg))))

(defun semacs--setup-permissions ()
  "Set correct permissions for SEMACS directories and files."
  (semacs--log-message "Setting up permissions...")

  ;; Directory permissions
  (mapc (lambda (dir)
          (set-file-modes dir #o755)
          (shell-command (format "sudo chown -R semacs:semacs %s" dir)))
        (list semacs-work-dir
              semacs-workspace-dir
              semacs-source-dir
              semacs-backups-dir
              semacs-logs-dir
              semacs-requests-dir
              semacs-config-dir))

  ;; Special file permissions
  (when (file-exists-p semacs-github-token-file)
    (set-file-modes semacs-github-token-file #o600))

  (dolist (file (directory-files-recursively semacs-logs-dir ".*\\.log$"))
    (set-file-modes file #o644)))

(defun semacs--backup-existing-files ()
  "Backup existing SEMACS files if they exist."
  (when (file-exists-p semacs-work-dir)
    (let ((backup-dir (format "%s/semacs-backup-%s"
                             temporary-file-directory
                             (format-time-string "%Y%m%d-%H%M%S"))))
      (make-directory backup-dir t)
      (copy-directory semacs-work-dir backup-dir t t t)
      (semacs--log-message
       (format "Existing files backed up to %s" backup-dir)))))

;; (defun semacs--create-directories ()
;;   "Create all necessary directories for SEMACS."
;;   (mapc (lambda (dir)
;;           (unless (file-exists-p dir)
;;             (make-directory dir t)))
;;         (list semacs-work-dir
;;               semacs-workspace-dir
;;               semacs-source-dir
;;               semacs-backups-dir
;;               semacs-logs-dir
;;               semacs-requests-dir
;;               semacs-config-dir)))

(defun semacs--create-initial-files ()
  "Create initial files and templates."
  (semacs--log-message "Creating initial files...")

  ;; Create user request template
  (with-temp-file semacs-user-request-file
    (insert "# Improvement Request\n\n"
            "## Description\n\n"
            "## Expected Outcome\n\n"
            "## Additional Notes\n"))

  ;; Create SEMACS request template
  (with-temp-file semacs-request-file
    (insert "# Self-Improvement Proposal\n\n"
            "## Current Limitation\n\n"
            "## Proposed Changes\n\n"
            "## Implementation Plan\n\n"
            "## Testing Strategy\n"))

  ;; Initialize history log
  (unless (file-exists-p semacs-history-file)
    (with-temp-file semacs-history-file
      (insert (format-time-string "# SEMACS History Log\nInitialized on %Y-%m-%d %H:%M:%S\n\n")))))

;; (defun semacs--verify-installation ()
;;   "Verify that all components are properly installed and configured."
;;   (semacs--log-message "Verifying installation...")

;;   (let ((checks
;;          `((,semacs-work-dir "Main working directory")
;;            (,semacs-workspace-dir "Workspace directory")
;;            (,semacs-source-dir "Source directory")
;;            (,semacs-logs-dir "Logs directory")
;;            (,semacs-config-dir "Config directory")
;;            (,semacs-github-token-file "GitHub token file")
;;            (,semacs-user-request-file "User request template")
;;            (,semacs-request-file "SEMACS request template")
;;            (,semacs-history-file "History log"))))

;;     (cl-loop for (path desc) in checks
;;              do (unless (file-exists-p path)
;;                   (error "Missing %s at %s" desc path))))

;;   (semacs--log-message "Installation verified successfully"))

(defun semacs--setup-environment ()
  "Set up SEMACS environment variables and shell configuration."
  (semacs--log-message "Setting up environment...")

  (let ((env-file (expand-file-name ".env" semacs-config-dir)))
    (with-temp-file env-file
      (insert (format "SEMACS_ROOT=%s\n" semacs-work-dir)
              (format "SEMACS_WORKSPACE=%s\n" semacs-workspace-dir)
              (format "SEMACS_SOURCE=%s\n" semacs-source-dir)
              (format "SEMACS_LOGS=%s\n" semacs-logs-dir)
              (format "SEMACS_CONFIG=%s\n" semacs-config-dir)))
    (shell-command (format "sudo chmod 644 %s" env-file))))

(defun semacs-install (&optional main-user)
  "Install SEMACS system with MAIN-USER as primary user.
If MAIN-USER is nil, use current user."
  (interactive)

  (let ((main-user (or main-user (user-login-name))))
    (condition-case err
        (progn
          (semacs--log-message "Starting SEMACS installation...")

          ;; Core setup
          (semacs--setup-user main-user)
          ;; Directories and files
          (semacs--setup-workspace)

          ;; Pre-installation checks
          (semacs--check-dependencies)

          ;; Git/GitHub
          (semacs--setup-git-config)
          (semacs--setup-github-token)

          ;; Repository and dependencies
          (semacs--install-dependencies)

          ;; Configuration and files
          (semacs--setup-permissions)
          (semacs--create-initial-files)
          (semacs--setup-environment)

          ;; Final verification
          (semacs-verify-installation)

          (semacs-setup-sudo)

          (semacs--log-message "SEMACS installation completed successfully!")
          t)

      (error
       (semacs--log-message (format "Installation failed: %s" (error-message-string err)))
       nil))))

(provide 'semacs-install)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; (require 'semacs)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
