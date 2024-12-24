;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-06 01:23:22
;;; Time-stamp: <2024-12-06 01:23:22 (ywatanabe)>
;;; File: ./self-evolving-agent/src/elmo-install.el


(require 'elmo-config)
(require 'cl-lib)
(require 'auth-source)
(require 'elmo-verify-installation)
(require 'elmo-logging)


(defun elmo-setup-sudo ()
  "Setup sudo configuration for ELMO."
  (interactive)
  (let ((sudo-file "/etc/sudoers.d/elmo-emacs")
        (temp-file (make-temp-file "elmo-sudo"))
        (content (format "%s ALL=(%s) NOPASSWD: %s\n"
                        (user-login-name)
                        elmo-user
                        elmo-emacs-cli)))
    (write-region content nil temp-file)
    (call-process "sudo" nil nil nil
                 "cp" temp-file sudo-file)
    (call-process "sudo" nil nil nil
                 "chown" "root:root" sudo-file)
    (call-process "sudo" nil nil nil
                 "chmod" "440" sudo-file)
    (delete-file temp-file)))

;; (elmo-setup-sudo)

;; # /etc/sudoers.d/elmo-emacs
;; ywatanabe ALL=(elmo) NOPASSWD: /usr/bin/emacsclient


(defun elmo--check-dependencies ()
  "Check if required system dependencies are available."
  (let ((required-commands '("git" "sudo" "python3"))
        (missing-commands '()))

    (dolist (cmd required-commands)
      (unless (executable-find cmd)
        (push cmd missing-commands)))

    (when missing-commands
      (error "Missing required commands: %s"
             (string-join missing-commands ", ")))))

;; (defun elmo--create-user (username)
;;   "Create a new system user for ELMO."
;;   (unless (zerop (shell-command
;;                   (format "id %s >/dev/null 2>&1" username)))
;;     (shell-command
;;      (format "sudo useradd -m -s /bin/bash %s" username))
;;     (shell-command
;;      (format "sudo usermod -aG sudo %s" username))))

(defun elmo--setup-workspace ()
  "Initialize ELMO workspace with symbolic links."
  (interactive)
  (let* ((user-name (user-login-name))
         (source-dir (directory-file-name elmo-user-root-dir))
         (workspace-dir (directory-file-name elmo-workspace-dir))
         (target-link (expand-file-name "self-evolving-agent" workspace-dir)))

    ;; Verify user is in elmo group
    (unless (member "elmo" (split-string (shell-command-to-string
                                       (format "groups %s" user-name))))
      (error "Current user must be in 'elmo' group. Run install.sh first"))

    ;; Create base directories
    (dolist (dir (list elmo-work-dir
                      elmo-workspace-dir
                      elmo-backups-dir
                      elmo-logs-dir
                      elmo-command-logs-dir
                      elmo-requests-dir
                      elmo-config-dir))
      (unless (file-exists-p dir)
        (make-directory dir t)
        (set-file-modes dir #o700)))

    ;; Touch request files
    (dolist (file (list elmo-user-request-file
                       elmo-request-file))
      (unless (file-exists-p file)
        (write-region "" nil file)))

    ;; Create symbolic link
    (when (file-exists-p target-link)
      (delete-file target-link))
    (make-symbolic-link source-dir target-link)))

(defun elmo--user-exists-p (username)
  "Check if USERNAME exists in the system."
  (zerop (shell-command
          (format "id %s >/dev/null 2>&1" username))))

(defun elmo--setup-user (main-user)
  "Set up ELMO user and configure permissions for MAIN-USER."
  (unless (elmo--user-exists-p main-user)
    (error "User %s does not exist" main-user))

  (unless (elmo--user-exists-p "elmo")
    (elmo--log-message "Creating elmo user...")
    (unless (zerop (shell-command "sudo useradd -r -m -d /home/elmo elmo"))
      (error "Failed to create elmo user"))
    (shell-command "sudo chmod 755 /home/elmo"))

  (elmo--log-message "Configuring groups...")
  (shell-command (format "sudo usermod -aG elmo %s" main-user))
  (shell-command (format "sudo usermod -aG %s elmo" main-user))
  )

(defun elmo--setup-git-config ()
  "Configure git settings for ELMO user."
  (elmo--log-message "Setting up git configuration for elmo user...")

  (let ((git-commands
         '("git config --global user.name \"elmo-bot\""
           "git config --global user.email \"elmo-bot@example.com\""
           "git config --global core.editor \"gedit\""
           "git config --global init.defaultBranch \"main\"")))
    (dolist (cmd git-commands)
      (shell-command (format "sudo -u elmo %s" cmd))))

  (let ((gitignore (expand-file-name ".gitignore_global" elmo-config-dir)))
    (with-temp-file gitignore
      (insert "*~\n.DS_Store\n.env\n*.log\n"))
    (shell-command (format "sudo -u elmo git config --global core.excludesfile %s" gitignore))
    (shell-command (format "sudo chmod 600 %s" gitignore))
    (elmo--log-message "Git configuration completed")))

;; (defun elmo--setup-github-token ()
;;   "Set up GitHub token interactively."
;;   (elmo--log-message "Setting up GitHub token...")

;;   (when (file-exists-p elmo-github-token-file)
;;     (let* ((default-token (with-temp-buffer
;;                            (insert-file-contents elmo-github-token-file)
;;                            (buffer-string)))
;;            (masked-token (concat (substring default-token 0 4)
;;                                "..."
;;                                (substring default-token -4))))
;;       (let ((input (read-string
;;                    (format "Enter GitHub Token (Enter for %s, s to skip): "
;;                           masked-token))))
;;         (cond ((string-empty-p input)
;;                (elmo--log-message "Keeping existing token")
;;                (cl-return-from elmo--setup-github-token t))
;;               ((string= input "s")
;;                (elmo--log-message "Skipping token setup")
;;                (cl-return-from elmo--setup-github-token t))))))

;;   (let ((token (read-string "Enter GitHub Personal Access Token (s to skip): ")))
;;     (when (string= token "s")
;;       (elmo--log-message "Skipping token setup")
;;       (cl-return-from elmo--setup-github-token t))

;;     (when (< (length token) 40)
;;       (elmo--log-message "Error: Invalid token length")
;;       (cl-return-from elmo--setup-github-token nil))

;;     (with-temp-file elmo-github-token-file
;;       (insert token))
;;     (shell-command (format "sudo chmod 600 %s" elmo-github-token-file))
;;     (elmo--log-message "GitHub token saved")))

(cl-defun elmo--setup-github-token ()
  "Set up GitHub token interactively."
  (elmo--log-message "Setting up GitHub token...")

  (when (file-exists-p elmo-github-token-file)
    (let* ((default-token (with-temp-buffer
                           (insert-file-contents elmo-github-token-file)
                           (buffer-string)))
           (masked-token (concat (substring default-token 0 4)
                               "..."
                               (substring default-token -4))))
      (let ((input (read-string
                   (format "Enter GitHub Token (Enter for %s, s to skip): "
                          masked-token))))
        (cond ((string-empty-p input)
               (elmo--log-message "Keeping existing token")
               (cl-return-from elmo--setup-github-token t))
              ((string= input "s")
               (elmo--log-message "Skipping token setup")
               (cl-return-from elmo--setup-github-token t))))))

  (let ((token (read-string "Enter GitHub Personal Access Token (s to skip): ")))
    (when (string= token "s")
      (elmo--log-message "Skipping token setup")
      (cl-return-from elmo--setup-github-token t))

    (when (< (length token) 40)
      (elmo--log-message "Error: Invalid token length")
      (cl-return-from elmo--setup-github-token nil))

    (with-temp-file elmo-github-token-file
      (insert token))
    (shell-command (format "sudo chmod 600 %s" elmo-github-token-file))
    (elmo--log-message "GitHub token saved")))



(defun elmo--install-dependencies ()
  "Install required system packages and Emacs packages."
  (elmo--log-message "Installing dependencies...")

  ;; System packages
  (let ((packages '("python3" "curl" "wget")))
    (dolist (pkg packages)
      (unless (zerop (shell-command (format "which %s >/dev/null 2>&1" pkg)))
        (elmo--log-message (format "Installing %s..." pkg))
        (let ((result (shell-command (format "sudo apt-get install -y %s" pkg))))
          (unless (zerop result)
            (display-warning 'elmo (format "Failed to install %s" pkg) :error))))))

  ;; Python packages
  (let* ((default-directory elmo-workspace-dir)
         (venv-dir (expand-file-name ".env" elmo-workspace-dir)))
    ;; Create and activate virtual environment
    (unless (file-exists-p venv-dir)
      (shell-command "python3 -m venv .env"))

    (let ((commands
           `(,(format "bash -c 'source %s/bin/activate && pip install --upgrade pip'" venv-dir)
             ,(format "bash -c 'source %s/bin/activate && pip install -r %s/requirements.txt'"
                     venv-dir elmo-user-root-dir))))
      (dolist (cmd commands)
        (let ((result (shell-command cmd)))
          (unless (zerop result)
            (display-warning 'elmo
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
               (display-warning 'elmo
                              (format "Failed to install package: %s" pkg)
                              :error))))))
    (error
     (display-warning 'elmo
                     (format "Error during Emacs package setup: %s" (error-message-string err))
                     :error))))

;; (defun elmo--install-dependencies ()
;;   "Install required system packages and Emacs packages."
;;   (elmo--log-message "Installing dependencies...")

;;   ;; System packages
;;   (let ((packages '("python3" "curl" "wget")))
;;     (dolist (pkg packages)
;;       (unless (zerop (shell-command (format "which %s >/dev/null 2>&1" pkg)))
;;         (elmo--log-message (format "Installing %s..." pkg))
;;         (shell-command (format "sudo apt-get install -y %s" pkg)))))

;;   ;; Python packages
;;   (shell-command "cd elmo-workspace-dir")
;;   (shell-command "python -m pip install -U pip")
;;   (shell-command "python -m venv .env")
;;   (shell-command "source .env/bin/activate")
;;   (shell-command "python -m pip install -U pip")
;;   (shell-command (concat "python -m pip install -r " elmo-user-root-dir "requirements.txt"))

;;   ;; Emacs packages
;;   (require 'package)
;;   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;   (package-initialize)
;;   (package-refresh-contents)

;;   (dolist (pkg '(markdown-mode request async))
;;     (unless (package-installed-p pkg)
;;       (package-install pkg))))

(defun elmo--setup-permissions ()
  "Set correct permissions for ELMO directories and files."
  (elmo--log-message "Setting up permissions...")

  ;; Directory permissions
  (mapc (lambda (dir)
          (set-file-modes dir #o755)
          (shell-command (format "sudo chown -R elmo:elmo %s" dir)))
        (list elmo-work-dir
              elmo-workspace-dir
              elmo-source-dir
              elmo-backups-dir
              elmo-logs-dir
              elmo-requests-dir
              elmo-config-dir))

  ;; Special file permissions
  (when (file-exists-p elmo-github-token-file)
    (set-file-modes elmo-github-token-file #o600))

  (dolist (file (directory-files-recursively elmo-logs-dir ".*\\.log$"))
    (set-file-modes file #o644)))

(defun elmo--backup-existing-files ()
  "Backup existing ELMO files if they exist."
  (when (file-exists-p elmo-work-dir)
    (let ((backup-dir (format "%s/elmo-backup-%s"
                             temporary-file-directory
                             (format-time-string "%Y%m%d-%H%M%S"))))
      (make-directory backup-dir t)
      (copy-directory elmo-work-dir backup-dir t t t)
      (elmo--log-message
       (format "Existing files backed up to %s" backup-dir)))))

;; (defun elmo--create-directories ()
;;   "Create all necessary directories for ELMO."
;;   (mapc (lambda (dir)
;;           (unless (file-exists-p dir)
;;             (make-directory dir t)))
;;         (list elmo-work-dir
;;               elmo-workspace-dir
;;               elmo-source-dir
;;               elmo-backups-dir
;;               elmo-logs-dir
;;               elmo-requests-dir
;;               elmo-config-dir)))

(defun elmo--create-initial-files ()
  "Create initial files and templates."
  (elmo--log-message "Creating initial files...")

  ;; Create user request template
  (with-temp-file elmo-user-request-file
    (insert "# Improvement Request\n\n"
            "## Description\n\n"
            "## Expected Outcome\n\n"
            "## Additional Notes\n"))

  ;; Create ELMO request template
  (with-temp-file elmo-request-file
    (insert "# Self-Improvement Proposal\n\n"
            "## Current Limitation\n\n"
            "## Proposed Changes\n\n"
            "## Implementation Plan\n\n"
            "## Testing Strategy\n"))

  ;; Initialize history log
  (unless (file-exists-p elmo-history-file)
    (with-temp-file elmo-history-file
      (insert (format-time-string "# ELMO History Log\nInitialized on %Y-%m-%d %H:%M:%S\n\n")))))

;; (defun elmo--verify-installation ()
;;   "Verify that all components are properly installed and configured."
;;   (elmo--log-message "Verifying installation...")

;;   (let ((checks
;;          `((,elmo-work-dir "Main working directory")
;;            (,elmo-workspace-dir "Workspace directory")
;;            (,elmo-source-dir "Source directory")
;;            (,elmo-logs-dir "Logs directory")
;;            (,elmo-config-dir "Config directory")
;;            (,elmo-github-token-file "GitHub token file")
;;            (,elmo-user-request-file "User request template")
;;            (,elmo-request-file "ELMO request template")
;;            (,elmo-history-file "History log"))))

;;     (cl-loop for (path desc) in checks
;;              do (unless (file-exists-p path)
;;                   (error "Missing %s at %s" desc path))))

;;   (elmo--log-message "Installation verified successfully"))

(defun elmo--setup-environment ()
  "Set up ELMO environment variables and shell configuration."
  (elmo--log-message "Setting up environment...")

  (let ((env-file (expand-file-name ".env" elmo-config-dir)))
    (with-temp-file env-file
      (insert (format "ELMO_ROOT=%s\n" elmo-work-dir)
              (format "ELMO_WORKSPACE=%s\n" elmo-workspace-dir)
              (format "ELMO_SOURCE=%s\n" elmo-source-dir)
              (format "ELMO_LOGS=%s\n" elmo-logs-dir)
              (format "ELMO_CONFIG=%s\n" elmo-config-dir)))
    (shell-command (format "sudo chmod 644 %s" env-file))))

(defun elmo-install (&optional main-user)
  "Install ELMO system with MAIN-USER as primary user.
If MAIN-USER is nil, use current user."
  (interactive)

  (let ((main-user (or main-user (user-login-name))))
    (condition-case err
        (progn
          (elmo--log-message "Starting ELMO installation...")

          ;; Core setup
          (elmo--setup-user main-user)
          ;; Directories and files
          (elmo--setup-workspace)

          ;; Pre-installation checks
          (elmo--check-dependencies)

          ;; Git/GitHub
          (elmo--setup-git-config)
          (elmo--setup-github-token)

          ;; Repository and dependencies
          (elmo--install-dependencies)

          ;; Configuration and files
          (elmo--setup-permissions)
          (elmo--create-initial-files)
          (elmo--setup-environment)

          ;; Final verification
          (elmo-verify-installation)

          (elmo-setup-sudo)

          (elmo--log-message "ELMO installation completed successfully!")
          t)

      (error
       (elmo--log-message (format "Installation failed: %s" (error-message-string err)))
       nil))))

(provide 'elmo-install)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

;; (require 'elmo)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
