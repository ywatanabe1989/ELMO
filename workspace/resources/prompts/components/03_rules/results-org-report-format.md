<!-- ---
!-- title: 2025-01-03 05:26:51
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompt-templates/components/03_rules/results-org-report-format.md
!-- --- -->

# Rule: report-format
* Summarize all the outputs (e.g., code, data, images...) into an org file
* Save the org file under `/workspace/projects/<project-id>-<-project-name>/results/`, with appropriate directory structure.
* Show the org file into the buffer popup-displayed by this code: `(llemacs--buffer-display llemacs--buffer-main)`
* All results, including code, media, report, should be linked in the org content.
* Also, convert the org file to pdf
* Add links to all the data produced in the org and pdf files.
* Images should be displayed inline mode. This may be useful:
  * `(setq org-startup-with-inline-images t)`
  * `org-toggle-inline-images`
* IMAGE_WIDTH should be 400
* Insert the directory path of the org file at the top of the contents