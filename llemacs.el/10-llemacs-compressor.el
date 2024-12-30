;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-27 19:44:34
;;; Time-stamp: <2024-12-27 19:44:34 (ywatanabe)>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/llemacs/elisp/llemacs/10-llemacs-compressor.el

(require '01-llemacs-config)
(require '10-llemacs-run)

(defvar llemacs-compress-max-length 1024
  "Maximum length for compressed text output.")

(defun llemacs-compress-text (text &optional max-len)
  "Compress TEXT using LLM within MAX-LEN characters."
  (let ((limit (or max-len llemacs-compress-max-length)))
    (if (<= (length text) limit)
        text
      (llemacs-run
       (format "Compress to %d chars, preserve key information:\n%s"
               limit text)))))

(llemacs-compress-text "The hippocampus (pl.: hippocampi; via Latin from Greek ἱππόκαμπος, 'seahorse') is a major component of the brain of humans and other vertebrates. The hippocampus is part of the limbic system, and plays important roles in the consolidation of information from short-term memory to long-term memory, and in spatial memory that enables navigation. The hippocampus is located in the allocortex, with neural projections into the neocortex, in humans[1][2][3] as well as other primates.[4] The hippocampus, as the medial pallium, is a structure found in all vertebrates.[5] In humans, it contains two main interlocking parts: the hippocampus proper (also called Ammon's horn), and the dentate gyrus.[6][7]\n\nIn Alzheimer's disease (and other forms of dementia), the hippocampus is one of the first regions of the brain to suffer damage;[8] short-term memory loss and disorientation are included among the early symptoms. Damage to the hippocampus can also result from oxygen starvation (hypoxia), encephalitis, or medial temporal lobe epilepsy. People with extensive, bilateral hippocampal damage may experience anterograde amnesia: the inability to form and retain new memories.\n\nSince different neuronal cell types are neatly organized into layers in the hippocampus, it has frequently been used as a model system for studying neurophysiology. The form of neural plasticity known as long-term potentiation (LTP) was initially discovered to occur in the hippocampus and has often been studied in this structure. LTP is widely believed to be one of the main neural mechanisms by which memories are stored in the brain.\n\nIn rodents as model organisms, the hippocampus has been studied extensively as part of a brain system responsible for spatial memory and navigation. Many neurons in the rat and mouse hippocampus respond as place cells: that is, they fire bursts of action potentials when the animal passes through a specific part of its environment. Hippocampal place cells interact extensively with head direction cells, whose activity acts as an inertial compass, and conjecturally with grid cells in the neighboring entorhinal cortex.[citation needed]" 32)

;; (defun llemacs-compress-json (json-data &optional max-len)
;;   "Compress JSON-DATA using LLM summarization."
;;   (llemacs-compress-text (json-encode json-data) max-len))

;; (defun llemacs-compress-file (file &optional max-len)
;;   "Compress contents of FILE using LLM."
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (llemacs-compress-text (buffer-string) max-len)))

;; (defun llemacs-compress-dir (dir pattern &optional max-len)
;;   "Compress files matching PATTERN in DIR."
;;   (let ((files (directory-files dir t pattern))
;;         (compressed-dir (expand-file-name "compressed" dir)))
;;     (make-directory compressed-dir t)
;;     (mapcar
;;      (lambda (file)
;;        (let ((compressed (llemacs-compress-file file max-len))
;;              (comp-file (expand-file-name
;;                          (concat "comp-" (file-name-nondirectory file))
;;                          compressed-dir)))
;;          (with-temp-file comp-file
;;            (insert compressed))
;;          comp-file))
;;      files)))

;; (defun llemacs-compress-batch (texts &optional max-len)
;;   "Compress multiple TEXTS using LLM."
;;   (mapcar (lambda (text)
;;             (llemacs-compress-text text max-len))
;;           texts))

(provide '10-llemacs-compressor)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))