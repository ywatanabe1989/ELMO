;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 01:23:22
;;; Time-stamp: <2024-12-06 01:23:22 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-install.el


(require 'ninja-config)
(require 'cl-lib)
(require 'auth-source)
(require 'ninja-verify-installation)
(require 'ninja-logging)


(defun ninja-setup-sudo ()
  "Setup sudo configuration for NINJA."
  (interactive)
  (let ((sudo-file "/etc/sudoers.d/ninja-emacs")
        (temp-file (make-temp-file "ninja-sudo"))
        (content (format "%s ALL=(%s) NOPASSWD: %s\n"
                        (user-login-name)
                        ninja-user
                        ninja-emacs-cli)))
    (write-region content nil temp-file)
    (call-process "sudo" nil nil nil
                 "cp" temp-file sudo-file)
    (call-process "sudo" nil nil nil
                 "chown" "root:root" sudo-file)
    (call-process "sudo" nil nil nil
                 "chmod" "440" sudo-file)
    (delete-file temp-file)))

;; (ninja-setup-sudo)

;; # /etc/sudoers.d/ninja-emacs
;; ywatanabe ALL=(ninja) NOPASSWD: /usr/bin/emacsclient


(defun ninja--check-dependencies ()
  "Check if required system dependencies are available."
  (let ((required-commands '("git" "sudo" "python3"))
        (missing-commands '()))

    (dolist (cmd required-commands)
      (unless (executable-find cmd)
        (push cmd missing-commands)))

    (when missing-commands
      (error "Missing required commands: %s"
             (string-join missing-commands ", ")))))

;; (defun ninja--create-user (username)
;;   "Create a new system user for NINJA."
;;   (unless (zerop (shell-command
;;                   (format "id %s >/dev/null 2>&1" username)))
;;     (shell-command
;;      (format "sudo useradd -m -s /bin/bash %s" username))
;;     (shell-command
;;      (format "sudo usermod -aG sudo %s" username))))

(defun ninja--setup-workspace ()
  "Initialize NINJA workspace with symbolic links."
  (interactive)
  (let* ((user-name (user-login-name))
         (source-dir (directory-file-name ninja-user-root-dir))
         (workspace-dir (directory-file-name ninja-workspace-dir))
         (target-link (expand-file-name "self-evolving-agent" workspace-dir)))

    ;; Verify user is in ninja group
    (unless (member "ninja" (split-string (shell-command-to-string
                                       (format "groups %s" user-name))))
      (error "Current user must be in 'ninja' group. Run install.sh first"))

    ;; Create base directories
    (dolist (dir (list ninja-work-dir
                      ninja-workspace-dir
                      ninja-backups-dir
                      ninja-logs-dir
                      ninja-command-logs-dir
                      ninja-requests-dir
                      ninja-config-dir))
      (unless (file-exists-p dir)
        (make-directory dir t)
        (set-file-modes dir #o700)))

    ;; Touch request files
    (dolist (file (list ninja-user-request-file
                       ninja-request-file))
      (unless (file-exists-p file)
        (write-region "" nil file)))

    ;; Create symbolic link
    (when (file-exists-p target-link)
      (delete-file target-link))
    (make-symbolic-link source-dir target-link)))

(defun ninja--user-exists-p (username)
  "Check if USERNAME exists in the system."
  (zerop (shell-command
          (format "id %s >/dev/null 2>&1" username))))

(defun ninja--setup-user (main-user)
  "Set up NINJA user and configure permissions for MAIN-USER."
  (unless (ninja--user-exists-p main-user)
    (error "User %s does not exist" main-user))

  (unless (ninja--user-exists-p "ninja")
    (ninja--log-message "Creating ninja user...")
    (unless (zerop (shell-command "sudo useradd -r -m -d /home/ninja ninja"))
      (error "Failed to create ninja user"))
    (shell-command "sudo chmod 755 /home/ninja"))

  (ninja--log-message "Configuring groups...")
  (shell-command (format "sudo usermod -aG ninja %s" main-user))
  (shell-command (format "sudo usermod -aG %s ninja" main-user))
  )

(defun ninja--setup-git-config ()
  "Configure git settings for NINJA user."
  (ninja--log-message "Setting up git configuration for ninja user...")

  (let ((git-commands
         '("git config --global user.name \"ninja-bot\""
           "git config --global user.email \"ninja-bot@example.com\""
           "git config --global core.editor \"gedit\""
           "git config --global init.defaultBranch \"main\"")))
    (dolist (cmd git-commands)
      (shell-command (format "sudo -u ninja %s" cmd))))

  (let ((gitignore (expand-file-name ".gitignore_global" ninja-config-dir)))
    (with-temp-file gitignore
      (insert "*~\n.DS_Store\n.env\n*.log\n"))
    (shell-command (format "sudo -u ninja git config --global core.excludesfile %s" gitignore))
    (shell-command (format "sudo chmod 600 %s" gitignore))
    (ninja--log-message "Git configuration completed")))

;; (defun ninja--setup-github-token ()
;;   "Set up GitHub token interactively."
;;   (ninja--log-message "Setting up GitHub token...")

;;   (when (file-exists-p ninja-github-token-file)
;;     (let* ((default-token (with-temp-buffer
;;                            (insert-file-contents ninja-github-token-file)
;;                            (buffer-string)))
;;            (masked-token (concat (substring default-token 0 4)
;;                                "..."
;;                                (substring default-token -4))))
;;       (let ((input (read-string
;;                    (format "Enter GitHub Token (Enter for %s, s to skip): "
;;                           masked-token))))
;;         (cond ((string-empty-p input)
;;                (ninja--log-message "Keeping existing token")
;;                (cl-return-from ninja--setup-github-token t))
;;               ((string= input "s")
;;                (ninja--log-message "Skipping token setup")
;;                (cl-return-from ninja--setup-github-token t))))))

;;   (let ((token (read-string "Enter GitHub Personal Access Token (s to skip): ")))
;;     (when (string= token "s")
;;       (ninja--log-message "Skipping token setup")
;;       (cl-return-from ninja--setup-github-token t))

;;     (when (< (length token) 40)
;;       (ninja--log-message "Error: Invalid token length")
;;       (cl-return-from ninja--setup-github-token nil))

;;     (with-temp-file ninja-github-token-file
;;       (insert token))
;;     (shell-command (format "sudo chmod 600 %s" ninja-github-token-file))
;;     (ninja--log-message "GitHub token saved")))

(cl-defun ninja--setup-github-token ()
  "Set up GitHub token interactively."
  (ninja--log-message "Setting up GitHub token...")

  (when (file-exists-p ninja-github-token-file)
    (let* ((default-token (with-temp-buffer
                           (insert-file-contents ninja-github-token-file)
                           (buffer-string)))
           (masked-token (concat (substring default-token 0 4)
                               "..."
                               (substring default-token -4))))
      (let ((input (read-string
                   (format "Enter GitHub Token (Enter for %s, s to skip): "
                          masked-token))))
        (cond ((string-empty-p input)
               (ninja--log-message "Keeping existing token")
               (cl-return-from ninja--setup-github-token t))
              ((string= input "s")
               (ninja--log-message "Skipping token setup")
               (cl-return-from ninja--setup-github-token t))))))

  (let ((token (read-string "Enter GitHub Personal Access Token (s to skip): ")))
    (when (string= token "s")
      (ninja--log-message "Skipping token setup")
      (cl-return-from ninja--setup-github-token t))

    (when (< (length token) 40)
      (ninja--log-message "Error: Invalid token length")
      (cl-return-from ninja--setup-github-token nil))

    (with-temp-file ninja-github-token-file
      (insert token))
    (shell-command (format "sudo chmod 600 %s" ninja-github-token-file))
    (ninja--log-message "GitHub token saved")))



(defun ninja--install-dependencies ()
  "Install required system packages and Emacs packages."
  (ninja--log-message "Installing dependencies...")

  ;; System packages
  (let ((packages '("python3" "curl" "wget")))
    (dolist (pkg packages)
      (unless (zerop (shell-command (format "which %s >/dev/null 2>&1" pkg)))
        (ninja--log-message (format "Installing %s..." pkg))
        (let ((result (shell-command (format "sudo apt-get install -y %s" pkg))))
          (unless (zerop result)
            (display-warning 'ninja (format "Failed to install %s" pkg) :error))))))

  ;; Python packages
  (let* ((default-directory ninja-workspace-dir)
         (venv-dir (expand-file-name ".env" ninja-workspace-dir)))
    ;; Create and activate virtual environment
    (unless (file-exists-p venv-dir)
      (shell-command "python3 -m venv .env"))

    (let ((commands
           `(,(format "bash -c 'source %s/bin/activate && pip install --upgrade pip'" venv-dir)
             ,(format "bash -c 'source %s/bin/activate && pip install -r %s/requirements.txt'"
                     venv-dir ninja-user-root-dir))))
      (dolist (cmd commands)
        (let ((result (shell-command cmd)))
          (unless (zerop result)
            (display-warning 'ninja
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
               (display-warning 'ninja
                              (format "Failed to install package: %s" pkg)
                              :error))))))
    (error
     (display-warning 'ninja
                     (format "Error during Emacs package setup: %s" (error-message-string err))
                     :error))))

;; (defun ninja--install-dependencies ()
;;   "Install required system packages and Emacs packages."
;;   (ninja--log-message "Installing dependencies...")

;;   ;; System packages
;;   (let ((packages '("python3" "curl" "wget")))
;;     (dolist (pkg packages)
;;       (unless (zerop (shell-command (format "which %s >/dev/null 2>&1" pkg)))
;;         (ninja--log-message (format "Installing %s..." pkg))
;;         (shell-command (format "sudo apt-get install -y %s" pkg)))))

;;   ;; Python packages
;;   (shell-command "cd ninja-workspace-dir")
;;   (shell-command "python -m pip install -U pip")
;;   (shell-command "python -m venv .env")
;;   (shell-command "source .env/bin/activate")
;;   (shell-command "python -m pip install -U pip")
;;   (shell-command (concat "python -m pip install -r " ninja-user-root-dir "requirements.txt"))

;;   ;; Emacs packages
;;   (require 'package)
;;   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;   (package-initialize)
;;   (package-refresh-contents)

;;   (dolist (pkg '(markdown-mode request async))
;;     (unless (package-installed-p pkg)
;;       (package-install pkg))))

(defun ninja--setup-permissions ()
  "Set correct permissions for NINJA directories and files."
  (ninja--log-message "Setting up permissions...")

  ;; Directory permissions
  (mapc (lambda (dir)
          (set-file-modes dir #o755)
          (shell-command (format "sudo chown -R ninja:ninja %s" dir)))
        (list ninja-work-dir
              ninja-workspace-dir
              ninja-source-dir
              ninja-backups-dir
              ninja-logs-dir
              ninja-requests-dir
              ninja-config-dir))

  ;; Special file permissions
  (when (file-exists-p ninja-github-token-file)
    (set-file-modes ninja-github-token-file #o600))

  (dolist (file (directory-files-recursively ninja-logs-dir ".*\\.log$"))
    (set-file-modes file #o644)))

(defun ninja--backup-existing-files ()
  "Backup existing NINJA files if they exist."
  (when (file-exists-p ninja-work-dir)
    (let ((backup-dir (format "%s/ninja-backup-%s"
                             temporary-file-directory
                             (format-time-string "%Y%m%d-%H%M%S"))))
      (make-directory backup-dir t)
      (copy-directory ninja-work-dir backup-dir t t t)
      (ninja--log-message
       (format "Existing files backed up to %s" backup-dir)))))

;; (defun ninja--create-directories ()
;;   "Create all necessary directories for NINJA."
;;   (mapc (lambda (dir)
;;           (unless (file-exists-p dir)
;;             (make-directory dir t)))
;;         (list ninja-work-dir
;;               ninja-workspace-dir
;;               ninja-source-dir
;;               ninja-backups-dir
;;               ninja-logs-dir
;;               ninja-requests-dir
;;               ninja-config-dir)))

(defun ninja--create-initial-files ()
  "Create initial files and templates."
  (ninja--log-message "Creating initial files...")

  ;; Create user request template
  (with-temp-file ninja-user-request-file
    (insert "# Improvement Request\n\n"
            "## Description\n\n"
            "## Expected Outcome\n\n"
            "## Additional Notes\n"))

  ;; Create NINJA request template
  (with-temp-file ninja-request-file
    (insert "# Self-Improvement Proposal\n\n"
            "## Current Limitation\n\n"
            "## Proposed Changes\n\n"
            "## Implementation Plan\n\n"
            "## Testing Strategy\n"))

  ;; Initialize history log
  (unless (file-exists-p ninja-history-file)
    (with-temp-file ninja-history-file
      (insert (format-time-string "# NINJA History Log\nInitialized on %Y-%m-%d %H:%M:%S\n\n")))))

;; (defun ninja--verify-installation ()
;;   "Verify that all components are properly installed and configured."
;;   (ninja--log-message "Verifying installation...")

;;   (let ((checks
;;          `((,ninja-work-dir "Main working directory")
;;            (,ninja-workspace-dir "Workspace directory")
;;            (,ninja-source-dir "Source directory")
;;            (,ninja-logs-dir "Logs directory")
;;            (,ninja-config-dir "Config directory")
;;            (,ninja-github-token-file "GitHub token file")
;;            (,ninja-user-request-file "User request template")
;;            (,ninja-request-file "NINJA request template")
;;            (,ninja-history-file "History log"))))

;;     (cl-loop for (path desc) in checks
;;              do (unless (file-exists-p path)
;;                   (error "Missing %s at %s" desc path))))

;;   (ninja--log-message "Installation verified successfully"))

(defun ninja--setup-environment ()
  "Set up NINJA environment variables and shell configuration."
  (ninja--log-message "Setting up environment...")

  (let ((env-file (expand-file-name ".env" ninja-config-dir)))
    (with-temp-file env-file
      (insert (format "NINJA_ROOT=%s\n" ninja-work-dir)
              (format "NINJA_WORKSPACE=%s\n" ninja-workspace-dir)
              (format "NINJA_SOURCE=%s\n" ninja-source-dir)
              (format "NINJA_LOGS=%s\n" ninja-logs-dir)
              (format "NINJA_CONFIG=%s\n" ninja-config-dir)))
    (shell-command (format "sudo chmod 644 %s" env-file))))

(defun ninja-install (&optional main-user)
  "Install NINJA system with MAIN-USER as primary user.
If MAIN-USER is nil, use current user."
  (interactive)

  (let ((main-user (or main-user (user-login-name))))
    (condition-case err
        (progn
          (ninja--log-message "Starting NINJA installation...")

          ;; Core setup
          (ninja--setup-user main-user)
          ;; Directories and files
          (ninja--setup-workspace)

          ;; Pre-installation checks
          (ninja--check-dependencies)

          ;; Git/GitHub
          (ninja--setup-git-config)
          (ninja--setup-github-token)

          ;; Repository and dependencies
          (ninja--install-dependencies)

          ;; Configuration and files
          (ninja--setup-permissions)
          (ninja--create-initial-files)
          (ninja--setup-environment)

          ;; Final verification
          (ninja-verify-installation)

          (ninja-setup-sudo)

          (ninja--log-message "NINJA installation completed successfully!")
          t)

      (error
       (ninja--log-message (format "Installation failed: %s" (error-message-string err)))
       nil))))

(provide 'ninja-install)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; (require 'ninja)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
