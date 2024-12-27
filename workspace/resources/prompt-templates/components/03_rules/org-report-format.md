<!-- ---
!-- title: 2024-12-27 23:18:27
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/.emacs.d/lisp/elmo/workspace/resources/prompt-templates/components/03_rules/org-report-format.md
!-- --- -->

# Rule: report-format
* Summarize all the outputs (e.g., code, data, images...) as an org file
* Append the org file contents to the end of "*ELMO*" buffer.
* All results, including code, media, report, should be linked in the org content.
* Also, convert the org file to pdf
* Add links to all the data produced in the org and pdf files.
* Images should be displayed inline
* IMAGE_WIDTH should be 400
* GIF images should be displayed with `(org-inline-anim-mode 1)`
* Insert the directory path of the org file at the top of the contents