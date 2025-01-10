;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-11 09:18:21
;;; Timestamp: <2025-01-11 09:18:21>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/06-run-select.el

(defun llemacs--run-select-options-to-str (options)
  "Convert OPTIONS list to formatted string for prompt.

Example:
(llemacs--run-select-options-to-str '(\"opt1\" \"opt2\"))
;; => \"Available options:\\n1. opt1\\n2. opt2\""
  (let ((options-str "Available options:\n"))
    (concat options-str
            (mapconcat (lambda (i)
                         (format "%d. %s"
                                 (1+ i)
                                 (nth i options)))
                       (number-sequence 0 (1- (length options)))
                       "\n"))))

;; (llemacs--run-select-options-to-str '("analyze" "visualize" "report"))

(defun llemacs--parse-selection (response)
  "Parse selection response and return selected index."
  (when (string-match "Selected option: \\([0-9]\\)" response)
    (1- (string-to-number (match-string 1 response)))))

(defun llemacs--run-select (options)
  "Present OPTIONS list and run selected option via prompt.

Example:
(llemacs--run-select '(\"analyze\" \"visualize\" \"report\"))
;; => \"analyze\" "
  (let* ((options-prompt
          (format "Please select from the following options and return only single digit as \"selected option: X\" where X is a digit:\n%s"
                  (llemacs--run-select-options-to-str options)))
         (response (llemacs-llm options-prompt))
         (selected-idx (llemacs--parse-selection response)))
    (when (and selected-idx
               (>= selected-idx 0)
               (< selected-idx (length options)))
      (nth selected-idx options))))

;; (llemacs--run-select '("analyze" "visualize" "report"))

(defun llemacs-run-with-selection (option-func-alist)
  "Run function based on user selection from OPTION-FUNC-ALIST.
Example:
(let ((data \"input.txt\")
      (param 42))
  (llemacs-run-with-selection
   `((\"analyze\" . ,(lambda () (llemacs--run-analyze data)))
     (\"visualize\" . ,(lambda () (llemacs--run-visualize data param))))))"
  (let* ((options (mapcar #'car option-func-alist))
         (selected-option (llemacs--run-select options))
         (selected-func (cdr (assoc selected-option option-func-alist))))
    (when (functionp selected-func)
      (funcall selected-func))))

;; Example usage with different args per function:
;; (let ((data "input.txt")
;;       (param 42))
;;   (llemacs-run-with-selection
;;    `(("analyze" . ,(lambda () (llemacs--run-analyze data)))
;;      ("visualize" . ,(lambda () (llemacs--run-visualize data param))))))
