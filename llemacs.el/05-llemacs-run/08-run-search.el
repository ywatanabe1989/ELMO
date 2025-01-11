;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: 2025-01-12 01:48:05
;;; Timestamp: <2025-01-12 01:48:05>
;;; File: /home/ywatanabe/proj/llemacs/llemacs.el/05-llemacs-run/07-run-search.el

(require 'request)
(require 'json)

(defcustom llemacs--run-search-api-endpoint "https://api.search.brave.com/res/v1/web/search"
  "Endpoint URL for Brave Search API."
  :type 'string
  :group 'llemacs-run)

(defun llemacs--run-search (query)
  "Search web using Brave Search API with QUERY."
  (condition-case err
      (let ((response
             (request llemacs--run-search-api-endpoint
               :type "GET"
               :params `(("q" . ,query))
               :headers `(("Accept" . "application/json")
                          ("Accept-Encoding" . "gzip")
                          ("X-Subscription-Token" . ,(getenv "BRAVE_API_KEY")))
               :parser 'json-read
               :sync t)))
        (request-response-data response))
    (error
     (llemacs--logging-write-error-pj
      (format "Brave Search API request failed: %s" (error-message-string err))))))

(defun llemacs--run-search-extract-for-llm (response output-type)
  "Extract key information from search RESPONSE for LLM processing."
  (condition-case err
      (let ((raw-data (prin1-to-string response)))
        (pcase output-type
          ('alist (format "Here are the raw search results:\n%s\n\nPlease analyze this data and respond in Emacs Lisp alist format like: ((answer . \"...\") (source . \"...\") (confidence . high|medium|low))" raw-data))
          ('string (format "Here are the raw search results:\n%s\n\nPlease provide only a direct answer as string." raw-data))
          ('integer (format "Here are the raw search results:\n%s\n\nPlease provide only a numeric value as integer." raw-data))
          ('float (format "Here are the raw search results:\n%s\n\nPlease provide only a numeric value as float." raw-data))
          (_ (format "Here are the raw search results:\n%s\n\nPlease analyze this data and provide a direct answer." raw-data))))
    (error
     (llemacs--logging-write-error-pj
      (format "Failed to extract for LLM: %s" (error-message-string err))))))

(defun llemacs--run-search-with-llm (query &optional output-type)
  "Search and process QUERY with LLM. OUTPUT-TYPE can be 'alist, 'string, 'integer, or 'float."
  (let* ((response (llemacs--run-search query))
         (context (llemacs--run-search-extract-for-llm response output-type))
         (llm-response (llemacs-llm context)))
    (pcase output-type
      ('alist (read llm-response))
      ('integer (string-to-number llm-response))
      ('float (string-to-number llm-response))
      (_ llm-response))))

;; (llemacs--run-search-with-llm "temperature of Melbourne now in celcius degree" 'integer)
;; (llemacs--run-search-with-llm "temperature of Melbourne now in celcius degree" 'float)
;; (llemacs--run-search-with-llm "temperature of Melbourne now in celcius degree" 'string)
