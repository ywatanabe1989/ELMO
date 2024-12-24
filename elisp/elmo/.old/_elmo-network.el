;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-03 16:42:33
;;; Time-stamp: <2024-12-03 16:42:33 (ywatanabe)>
;;; File: ./self-evolving-agent/src/elmo-network.el


;;; Commentary:
;; Network functionality for self-evolving agent

;;; Code:

(require 'elmo-run)

(defvar elmo-server-port 8080
  "Port for agent server.")

(defvar elmo-agents nil
  "List of active agents.")

(cl-defstruct elmo-agent
  id task status)

(defun elmo-start-server ()
  "Start agent server."
  (interactive)
  (make-network-process
   :name "elmo-server"
   :buffer "*elmo-server*"
   :service elmo-server-port
   :family 'ipv4
   :server t
   :filter 'elmo-server-filter))

(defun elmo-server-filter (proc string)
  "Filter function for server process PROC with STRING input."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (when (string-match "\n" string)
      (let ((command (buffer-substring (point-min) (point-max))))
        (erase-buffer)
        (elmo-run command)))))

(defun elmo-spawn-agents (tasks)
  "Spawn multiple agents for TASKS."
  (dolist (task tasks)
    (push (make-elmo-agent :id (cl-gensym)
                         :task task
                         :status 'pending)
          elmo-agents))
  (elmo-coordinate-agents))

(defun elmo-coordinate-agents ()
  "Coordinate multiple agents' activities."
  (while elmo-agents
    (let ((agent (pop elmo-agents)))
      (elmo-show-progress
       (format "Agent %s processing: %s"
               (elmo-agent-id agent)
               (elmo-agent-task agent)))
      (elmo-run (elmo-agent-task agent)))))

(provide 'elmo-network)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
