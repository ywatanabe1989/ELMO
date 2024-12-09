;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-03 16:42:33
;;; Time-stamp: <2024-12-03 16:42:33 (ywatanabe)>
;;; File: ./self-evolving-agent/src/ninja-network.el


;;; Commentary:
;; Network functionality for self-evolving agent

;;; Code:

(require 'ninja-run)

(defvar ninja-server-port 8080
  "Port for agent server.")

(defvar ninja-agents nil
  "List of active agents.")

(cl-defstruct ninja-agent
  id task status)

(defun ninja-start-server ()
  "Start agent server."
  (interactive)
  (make-network-process
   :name "ninja-server"
   :buffer "*ninja-server*"
   :service ninja-server-port
   :family 'ipv4
   :server t
   :filter 'ninja--server-filter))

(defun ninja--server-filter (proc string)
  "Filter function for server process PROC with STRING input."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (when (string-match "\n" string)
      (let ((command (buffer-substring (point-min) (point-max))))
        (erase-buffer)
        (ninja-run command)))))

(defun ninja-spawn-agents (tasks)
  "Spawn multiple agents for TASKS."
  (dolist (task tasks)
    (push (make-ninja-agent :id (cl-gensym)
                         :task task
                         :status 'pending)
          ninja-agents))
  (ninja--coordinate-agents))

(defun ninja--coordinate-agents ()
  "Coordinate multiple agents' activities."
  (while ninja-agents
    (let ((agent (pop ninja-agents)))
      (ninja--show-progress
       (format "Agent %s processing: %s"
               (ninja-agent-id agent)
               (ninja-agent-task agent)))
      (ninja-run (ninja-agent-task agent)))))

(provide 'ninja-network)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
