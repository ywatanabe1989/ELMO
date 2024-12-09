;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-03 16:42:33
;;; Time-stamp: <2024-12-03 16:42:33 (ywatanabe)>
;;; File: ./self-evolving-agent/src/semacs-network.el


;;; Commentary:
;; Network functionality for self-evolving agent

;;; Code:

(require 'semacs-run)

(defvar semacs-server-port 8080
  "Port for agent server.")

(defvar semacs-agents nil
  "List of active agents.")

(cl-defstruct semacs-agent
  id task status)

(defun semacs-start-server ()
  "Start agent server."
  (interactive)
  (make-network-process
   :name "semacs-server"
   :buffer "*semacs-server*"
   :service semacs-server-port
   :family 'ipv4
   :server t
   :filter 'semacs--server-filter))

(defun semacs--server-filter (proc string)
  "Filter function for server process PROC with STRING input."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (when (string-match "\n" string)
      (let ((command (buffer-substring (point-min) (point-max))))
        (erase-buffer)
        (semacs-run command)))))

(defun semacs-spawn-agents (tasks)
  "Spawn multiple agents for TASKS."
  (dolist (task tasks)
    (push (make-semacs-agent :id (cl-gensym)
                         :task task
                         :status 'pending)
          semacs-agents))
  (semacs--coordinate-agents))

(defun semacs--coordinate-agents ()
  "Coordinate multiple agents' activities."
  (while semacs-agents
    (let ((agent (pop semacs-agents)))
      (semacs--show-progress
       (format "Agent %s processing: %s"
               (semacs-agent-id agent)
               (semacs-agent-task agent)))
      (semacs-run (semacs-agent-task agent)))))

(provide 'semacs-network)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
