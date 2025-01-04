;; Copyright (C) 2024-2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-03 16:42:33
;;; Time-stamp: <2024-12-03 16:42:33 (ywatanabe)>
;;; File: ./self-evolving-agent/src/llemacs-network.el


;;; Commentary:
;; Network functionality for self-evolving agent

;;; Code:

(require 'llemacs-run)

(defvar llemacs-server-port 8080
  "Port for agent server.")

(defvar llemacs-agents nil
  "List of active agents.")

(cl-defstruct llemacs-agent
  id task status)

(defun llemacs-start-server ()
  "Start agent server."
  (interactive)
  (make-network-process
   :name "llemacs-server"
   :buffer "*llemacs-server*"
   :service llemacs-server-port
   :family 'ipv4
   :server t
   :filter 'llemacs-server-filter))

(defun llemacs-server-filter (proc string)
  "Filter function for server process PROC with STRING input."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (when (string-match "\n" string)
      (let ((command (buffer-substring (point-min) (point-max))))
        (erase-buffer)
        (llemacs-run command)))))

(defun llemacs-spawn-agents (tasks)
  "Spawn multiple agents for TASKS."
  (dolist (task tasks)
    (push (make-llemacs-agent :id (cl-gensym)
                         :task task
                         :status 'pending)
          llemacs-agents))
  (llemacs-coordinate-agents))

(defun llemacs-coordinate-agents ()
  "Coordinate multiple agents' activities."
  (while llemacs-agents
    (let ((agent (pop llemacs-agents)))
      (llemacs-show-progress
       (format "Agent %s processing: %s"
               (llemacs-agent-id agent)
               (llemacs-agent-task agent)))
      (llemacs-run (llemacs-agent-task agent)))))

(provide 'llemacs-network)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
