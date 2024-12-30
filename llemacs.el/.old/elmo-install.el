;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 01:23:22
;;; Time-stamp: <2024-12-06 01:23:22 (ywatanabe)>
;;; File: ./self-evolving-agent/src/llemacs-install.el


(require 'llemacs-config)
(require 'cl-lib)
(require 'auth-source)
(require 'llemacs-verify-installation)
(require 'llemacs-logging)


(defun llemacs-setup-sudo ()
  "Setup sudo configuration for ELMO."
  (interactive)
  (let ((sudo-file "/etc/sudoers.d/llemacs-emacs")
        (temp-file (make-temp-file "llemacs-sudo"))
        (content (format "%s ALL=(%s) NOPASSWD: %s\n"
                        (user-login-name)
                        llemacs-user
                        llemacs-emacs-cli)))
    (write-region content nil temp-file)
    (call-process "sudo" nil nil nil
                 "cp" temp-file sudo-file)
    (call-process "sudo" nil nil nil
                 "chown" "root:root" sudo-file)
    (call-process "sudo" nil nil nil
                 "chmod" "440" sudo-file)
    (delete-file temp-file)))

;; (llemacs-setup-sudo)

;; # /etc/sudoers.d/llemacs-emacs
;; ywatanabe ALL=(llemacs) NOPASSWD: /usr/bin/emacsclient


(defun llemacs--check-dependencies ()
  "Check if required system dependencies are available."
  (let ((required-commands '("git" "sudo" "python3"))
        (missing-commands '()))

    (dolist (cmd required-commands)
      (unless (executable-find cmd)
        (push cmd missing-commands)))

    (when missing-commands
      (error "Missing required commands: %s"
             (string-join missing-commands ", ")))))

;; (defun llemacs--create-user (username)
;;   "Create a new system user for ELMO."
;;   (unless (zerop (shell-command
;;                   (format "id %s >/dev/null 2>&1" username)))
;;     (shell-command
;;      (format "sudo useradd -m -s /bin/bash %s" username))
;;     (shell-command
;;      (format "sudo usermod -aG sudo %s" username))))

(defun llemacs--setup-workspace ()
  "Initialize ELMO workspace with symbolic links."
  (interactive)
  (let* ((user-name (user-login-name))
         (source-dir (directory-file-name llemacs-user-root-dir))
         (workspace-dir (directory-file-name llemacs-workspace-dir))
         (target-link (expand-file-name "self-evolving-agent" workspace-dir)))

    ;; Verify user is in llemacs group
    (unless (member "llemacs" (split-string (shell-command-to-string
                                       (format "groups %s" user-name))))
      (error "Current user must be in 'llemacs' group. Run install.sh first"))

    ;; Create base directories
    (dolist (dir (list llemacs-work-dir
                      llemacs-workspace-dir
                      llemacs-backups-dir
                      llemacs-logs-dir
                      llemacs-command-logs-dir
                      llemacs-requests-dir
                      llemacs-config-dir))
      (unless (file-exists-p dir)
        (make-directory dir t)
        (set-file-modes dir #o700)))

    ;; Touch request files
    (dolist (file (list llemacs-user-request-file
                       llemacs-request-file))
      (unless (file-exists-p file)
        (write-region "" nil file)))

    ;; Create symbolic link
    (when (file-exists-p target-link)
      (delete-file target-link))
    (make-symbolic-link source-dir target-link)))

(defun llemacs--user-exists-p (username)
  "Check if USERNAME exists in the system."
  (zerop (shell-command
          (format "id %s >/dev/null 2>&1" username))))

(defun llemacs--setup-user (main-user)
  "Set up ELMO user and configure permissions for MAIN-USER."
  (unless (llemacs--user-exists-p main-user)
    (error "User %s does not exist" main-user))

  (unless (llemacs--user-exists-p "llemacs")
    (llemacs--log-message "Creating llemacs user...")
    (unless (zerop (shell-command "sudo useradd -r -m -d /home/llemacs llemacs"))
      (error "Failed to create llemacs user"))
    (shell-command "sudo chmod 755 /home/llemacs"))

  (llemacs--log-message "Configuring groups...")
  (shell-command (format "sudo usermod -aG llemacs %s" main-user))
  (shell-command (format "sudo usermod -aG %s llemacs" main-user))
  )

(defun llemacs--setup-git-config ()
  "Configure git settings for ELMO user."
  (llemacs--log-message "Setting up git configuration for llemacs user...")

  (let ((git-commands
         '("git config --global user.name \"llemacs-bot\""
           "git config --global user.email \"llemacs-bot@example.com\""
           "git config --global core.editor \"gedit\""
           "git config --global init.defaultBranch \"main\"")))
    (dolist (cmd git-commands)
      (shell-command (format "sudo -u llemacs %s" cmd))))

  (let ((gitignore (expand-file-name ".gitignore_global" llemacs-config-dir)))
    (with-temp-file gitignore
      (insert "*~\n.DS_Store\n.env\n*.log\n"))
    (shell-command (format "sudo -u llemacs git config --global core.excludesfile %s" gitignore))
    (shell-command (format "sudo chmod 600 %s" gitignore))
    (llemacs--log-message "Git configuration completed")))

;; (defun llemacs--setup-github-token ()
;;   "Set up GitHub token interactively."
;;   (llemacs--log-message "Setting up GitHub token...")

;;   (when (file-exists-p llemacs-github-token-file)
;;     (let* ((default-token (with-temp-buffer
;;                            (insert-file-contents llemacs-github-token-file)
;;                            (buffer-string)))
;;            (masked-token (concat (substring default-token 0 4)
;;                                "..."
;;                                (substring default-token -4))))
;;       (let ((input (read-string
;;                    (format "Enter GitHub Token (Enter for %s, s to skip): "
;;                           masked-token))))
;;         (cond ((string-empty-p input)
;;                (llemacs--log-message "Keeping existing token")
;;                (cl-return-from llemacs--setup-github-token t))
;;               ((string= input "s")
;;                (llemacs--log-message "Skipping token setup")
;;                (cl-return-from llemacs--setup-github-token t))))))

;;   (let ((token (read-string "Enter GitHub Personal Access Token (s to skip): ")))
;;     (when (string= token "s")
;;       (llemacs--log-message "Skipping token setup")
;;       (cl-return-from llemacs--setup-github-token t))

;;     (when (< (length token) 40)
;;       (llemacs--log-message "Error: Invalid token length")
;;       (cl-return-from llemacs--setup-github-token nil))

;;     (with-temp-file llemacs-github-token-file
;;       (insert token))
;;     (shell-command (format "sudo chmod 600 %s" llemacs-github-token-file))
;;     (llemacs--log-message "GitHub token saved")))

(cl-defun llemacs--setup-github-token ()
  "Set up GitHub token interactively."
  (llemacs--log-message "Setting up GitHub token...")

  (when (file-exists-p llemacs-github-token-file)
    (let* ((default-token (with-temp-buffer
                           (insert-file-contents llemacs-github-token-file)
                           (buffer-string)))
           (masked-token (concat (substring default-token 0 4)
                               "..."
                               (substring default-token -4))))
      (let ((input (read-string
                   (format "Enter GitHub Token (Enter for %s, s to skip): "
                          masked-token))))
        (cond ((string-empty-p input)
               (llemacs--log-message "Keeping existing token")
               (cl-return-from llemacs--setup-github-token t))
              ((string= input "s")
               (llemacs--log-message "Skipping token setup")
               (cl-return-from llemacs--setup-github-token t))))))

  (let ((token (read-string "Enter GitHub Personal Access Token (s to skip): ")))
    (when (string= token "s")
      (llemacs--log-message "Skipping token setup")
      (cl-return-from llemacs--setup-github-token t))

    (when (< (length token) 40)
      (llemacs--log-message "Error: Invalid token length")
      (cl-return-from llemacs--setup-github-token nil))

    (with-temp-file llemacs-github-token-file
      (insert token))
    (shell-command (format "sudo chmod 600 %s" llemacs-github-token-file))
    (llemacs--log-message "GitHub token saved")))



(defun llemacs--install-dependencies ()
  "Install required system packages and Emacs packages."
  (llemacs--log-message "Installing dependencies...")

  ;; System packages
  (let ((packages '("python3" "curl" "wget")))
    (dolist (pkg packages)
      (unless (zerop (shell-command (format "which %s >/dev/null 2>&1" pkg)))
        (llemacs--log-message (format "Installing %s..." pkg))
        (let ((result (shell-command (format "sudo apt-get install -y %s" pkg))))
          (unless (zerop result)
            (display-warning 'llemacs (format "Failed to install %s" pkg) :error))))))

  ;; Python packages
  (let* ((default-directory llemacs-workspace-dir)
         (venv-dir (expand-file-name ".env" llemacs-workspace-dir)))
    ;; Create and activate virtual environment
    (unless (file-exists-p venv-dir)
      (shell-command "python3 -m venv .env"))

    (let ((commands
           `(,(format "bash -c 'source %s/bin/activate && pip install --upgrade pip'" venv-dir)
             ,(format "bash -c 'source %s/bin/activate && pip install -r %s/requirements.txt'"
                     venv-dir llemacs-user-root-dir))))
      (dolist (cmd commands)
        (let ((result (shell-command cmd)))
          (unless (zerop result)
            (display-warning 'llemacs
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
               (display-warning 'llemacs
                              (format "Failed to install package: %s" pkg)
                              :error))))))
    (error
     (display-warning 'llemacs
                     (format "Error during Emacs package setup: %s" (error-message-string err))
                     :error))))

;; (defun llemacs--install-dependencies ()
;;   "Install required system packages and Emacs packages."
;;   (llemacs--log-message "Installing dependencies...")

;;   ;; System packages
;;   (let ((packages '("python3" "curl" "wget")))
;;     (dolist (pkg packages)
;;       (unless (zerop (shell-command (format "which %s >/dev/null 2>&1" pkg)))
;;         (llemacs--log-message (format "Installing %s..." pkg))
;;         (shell-command (format "sudo apt-get install -y %s" pkg)))))

;;   ;; Python packages
;;   (shell-command "cd llemacs-workspace-dir")
;;   (shell-command "python -m pip install -U pip")
;;   (shell-command "python -m venv .env")
;;   (shell-command "source .env/bin/activate")
;;   (shell-command "python -m pip install -U pip")
;;   (shell-command (concat "python -m pip install -r " llemacs-user-root-dir "requirements.txt"))

;;   ;; Emacs packages
;;   (require 'package)
;;   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;   (package-initialize)
;;   (package-refresh-contents)

;;   (dolist (pkg '(markdown-mode request async))
;;     (unless (package-installed-p pkg)
;;       (package-install pkg))))

(defun llemacs--setup-permissions ()
  "Set correct permissions for ELMO directories and files."
  (llemacs--log-message "Setting up permissions...")

  ;; Directory permissions
  (mapc (lambda (dir)
          (set-file-modes dir #o755)
          (shell-command (format "sudo chown -R llemacs:llemacs %s" dir)))
        (list llemacs-work-dir
              llemacs-workspace-dir
              llemacs-source-dir
              llemacs-backups-dir
              llemacs-logs-dir
              llemacs-requests-dir
              llemacs-config-dir))

  ;; Special file permissions
  (when (file-exists-p llemacs-github-token-file)
    (set-file-modes llemacs-github-token-file #o600))

  (dolist (file (directory-files-recursively llemacs-logs-dir ".*\\.log$"))
    (set-file-modes file #o644)))

(defun llemacs--backup-existing-files ()
  "Backup existing ELMO files if they exist."
  (when (file-exists-p llemacs-work-dir)
    (let ((backup-dir (format "%s/llemacs-backup-%s"
                             temporary-file-directory
                             (format-time-string "%Y%m%d-%H%M%S"))))
      (make-directory backup-dir t)
      (copy-directory llemacs-work-dir backup-dir t t t)
      (llemacs--log-message
       (format "Existing files backed up to %s" backup-dir)))))

;; (defun llemacs--create-directories ()
;;   "Create all necessary directories for ELMO."
;;   (mapc (lambda (dir)
;;           (unless (file-exists-p dir)
;;             (make-directory dir t)))
;;         (list llemacs-work-dir
;;               llemacs-workspace-dir
;;               llemacs-source-dir
;;               llemacs-backups-dir
;;               llemacs-logs-dir
;;               llemacs-requests-dir
;;               llemacs-config-dir)))

(defun llemacs--create-initial-files ()
  "Create initial files and templates."
  (llemacs--log-message "Creating initial files...")

  ;; Create user request template
  (with-temp-file llemacs-user-request-file
    (insert "# Improvement Request\n\n"
            "## Description\n\n"
            "## Expected Outcome\n\n"
            "## Additional Notes\n"))

  ;; Create ELMO request template
  (with-temp-file llemacs-request-file
    (insert "# Self-Improvement Proposal\n\n"
            "## Current Limitation\n\n"
            "## Proposed Changes\n\n"
            "## Implementation Plan\n\n"
            "## Testing Strategy\n"))

  ;; Initialize history log
  (unless (file-exists-p llemacs-history-file)
    (with-temp-file llemacs-history-file
      (insert (format-time-string "# ELMO History Log\nInitialized on %Y-%m-%d %H:%M:%S\n\n")))))

;; (defun llemacs--verify-installation ()
;;   "Verify that all components are properly installed and configured."
;;   (llemacs--log-message "Verifying installation...")

;;   (let ((checks
;;          `((,llemacs-work-dir "Main working directory")
;;            (,llemacs-workspace-dir "Workspace directory")
;;            (,llemacs-source-dir "Source directory")
;;            (,llemacs-logs-dir "Logs directory")
;;            (,llemacs-config-dir "Config directory")
;;            (,llemacs-github-token-file "GitHub token file")
;;            (,llemacs-user-request-file "User request template")
;;            (,llemacs-request-file "ELMO request template")
;;            (,llemacs-history-file "History log"))))

;;     (cl-loop for (path desc) in checks
;;              do (unless (file-exists-p path)
;;                   (error "Missing %s at %s" desc path))))

;;   (llemacs--log-message "Installation verified successfully"))

(defun llemacs--setup-environment ()
  "Set up ELMO environment variables and shell configuration."
  (llemacs--log-message "Setting up environment...")

  (let ((env-file (expand-file-name ".env" llemacs-config-dir)))
    (with-temp-file env-file
      (insert (format "ELMO_ROOT=%s\n" llemacs-work-dir)
              (format "ELMO_WORKSPACE=%s\n" llemacs-workspace-dir)
              (format "ELMO_SOURCE=%s\n" llemacs-source-dir)
              (format "ELMO_LOGS=%s\n" llemacs-logs-dir)
              (format "ELMO_CONFIG=%s\n" llemacs-config-dir)))
    (shell-command (format "sudo chmod 644 %s" env-file))))

(defun llemacs-install (&optional main-user)
  "Install ELMO system with MAIN-USER as primary user.
If MAIN-USER is nil, use current user."
  (interactive)

  (let ((main-user (or main-user (user-login-name))))
    (condition-case err
        (progn
          (llemacs--log-message "Starting ELMO installation...")

          ;; Core setup
          (llemacs--setup-user main-user)
          ;; Directories and files
          (llemacs--setup-workspace)

          ;; Pre-installation checks
          (llemacs--check-dependencies)

          ;; Git/GitHub
          (llemacs--setup-git-config)
          (llemacs--setup-github-token)

          ;; Repository and dependencies
          (llemacs--install-dependencies)

          ;; Configuration and files
          (llemacs--setup-permissions)
          (llemacs--create-initial-files)
          (llemacs--setup-environment)

          ;; Final verification
          (llemacs-verify-installation)

          (llemacs-setup-sudo)

          (llemacs--log-message "ELMO installation completed successfully!")
          t)

      (error
       (llemacs--log-message (format "Installation failed: %s" (error-message-string err)))
       nil))))

(provide 'llemacs-install)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; (require 'llemacs)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
