<!-- ---
!-- Timestamp: 2025-01-09 05:39:23
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/04_examples/code-elisp-progn-block.md
!-- --- -->


# Example Output: code-elisp-progn-block
``` elisp
(progn
  (let* ((title "data-analysis-example")
         (python-path llemacs--path-pj-python)
         (script-path (llemacs--path-pj-get-or-create-script-python "analyze_data.py"))
         (data-path (llemacs--path-pj-get-or-create-data "raw/sample_data.csv"))
         (dist-plot-path (llemacs--path-pj-get-or-create-figure "distribution.png" title))
         (time-plot-path (llemacs--path-pj-get-or-create-figure "time_series.png" title))
         (corre-plot-path (llemacs--path-pj-get-or-create-figure "correlation.png" title))
         (stats-table (llemacs--path-pj-get-or-create-table "summary_stats.csv" title))
         (results-table (llemacs--path-pj-get-or-create-table "analysis_results.csv" title))
         (org-file (llemacs--path-pj-get-or-create-report-org title))
         (pdf-file (llemacs--path-pj-get-or-create-report-pdf title))
         (width 600)
         (script-args (format (concat "--data %s "
                                    "--dist-plot-path %s "
                                    "--time-plot-path %s "
                                    "--corre-plot-path %s "
                                    "--stats-out %s "
                                    "--results-out %s "
                                    )
                             data-path
                             dist-plot-path 
                             time-plot-path 
                             corre-plot-path
                             stats-table 
                             results-table
                             )))
    
    ;; Validate paths
    (when (llemacs--validate-paths 
           (list data-path script-path))

        (llemacs--run-shell-command (format "%s %s %s" python-path script-path script-args))
        
        (with-temp-file org-file
          (llemacs--org-write-standard-headers title)
          (insert "* Data Analysis Report\n\n")
          (insert "** Input Data\n")
          (insert (format "Data source: [[file:%s]]\n\n" data-path))
          (insert "** Distribution Analysis\n")
          (insert "Distribution of key variables:\n")
          (llemacs--org-write-figure dist-plot-path width)
          (insert "** Time Series Analysis\n")
          (insert "Temporal patterns in the data:\n")
          (llemacs--org-write-figure time-plot-path width)
          (insert "** Correlation Analysis\n")
          (insert "Relationships between variables:\n")
          (llemacs--org-write-figure corre-plot-path width)
          (insert "** Statistical Summary\n")
          (insert "#+begin_src R :results table :exports results\n")
          (insert (format "read.csv('%s')\n" stats-table))
          (insert "#+end_src\n\n")
          (insert "** Analysis Results\n")
          (insert "#+begin_src R :results table :exports results\n")
          (insert (format "read.csv('%s')\n" results-table))
          (insert "#+end_src\n\n"))
        
        (let ((buf (llemacs--org-export-to-pdf org-file pdf-file)))
          (llemacs--org-setup-visualization buf pdf-file)
          (pop-to-buffer buf))))))
```
