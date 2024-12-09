;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-03 16:42:33
;;; Time-stamp: <2024-12-03 16:42:33 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-network.el


;;; Commentary:
;; Network functionality for self-evolving agent

;;; Code:

(require 'sea-run)

(defvar sea-server-port 8080
  "Port for agent server.")

(defvar sea-agents nil
  "List of active agents.")

(cl-defstruct sea-agent
  id task status)

(defun sea-start-server ()
  "Start agent server."
  (interactive)
  (make-network-process
   :name "sea-server"
   :buffer "*sea-server*"
   :service sea-server-port
   :family 'ipv4
   :server t
   :filter 'sea--server-filter))

(defun sea--server-filter (proc string)
  "Filter function for server process PROC with STRING input."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (when (string-match "\n" string)
      (let ((command (buffer-substring (point-min) (point-max))))
        (erase-buffer)
        (sea-run command)))))

(defun sea-spawn-agents (tasks)
  "Spawn multiple agents for TASKS."
  (dolist (task tasks)
    (push (make-sea-agent :id (cl-gensym)
                         :task task
                         :status 'pending)
          sea-agents))
  (sea--coordinate-agents))

(defun sea--coordinate-agents ()
  "Coordinate multiple agents' activities."
  (while sea-agents
    (let ((agent (pop sea-agents)))
      (sea--show-progress
       (format "Agent %s processing: %s"
               (sea-agent-id agent)
               (sea-agent-task agent)))
      (sea-run (sea-agent-task agent)))))

(provide 'sea-network)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
