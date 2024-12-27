<!-- ---
!-- title: 2024-12-27 23:18:35
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/.emacs.d/lisp/elmo/workspace/resources/prompt-templates/components/04_example_input_output/elisp.md
!-- --- -->

# Example Output: elisp
```elisp
(progn
  (let* ((title "sample-plot")
         (timestamp (format-time-string (concat "%Y%m%d-%H%M%S-" title)))
         (user-dir (expand-file-name (user-login-name) "/workspace"))
         (work-dir (expand-file-name timestamp user-dir))
         (script-path (expand-file-name "plot.py" work-dir))
         (image-path (expand-file-name "plot.jpg" work-dir))
         (org-file (expand-file-name "report.org" work-dir))
         (pdf-file (expand-file-name "report.pdf" work-dir))
         (width 400)
         (py-code "
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
plt.figure(figsize=(12, 8), dpi=100)
x = np.linspace(0, 10, 100)
y = np.sin(x)
plt.plot(x, y)
plt.xlabel('x')
plt.ylabel('sin(x)')
plt.title('Simple Plot')
plt.grid(True)
plt.savefig('plot.jpg', bbox_inches='tight')
"))
    (make-directory work-dir t)
    (cd work-dir)
    (with-temp-file script-path
      (insert py-code))
    (shell-command (format "cd %s && source /workspace/.env/bin/activate && python3 %s"
                          work-dir script-path))
    (sleep-for 1)
    (with-temp-file org-file

      (insert (format "#+TITLE: ELMO Report\n"))
      (insert (format "#+DATE: %s\n\n" timestamp))
      (insert (format "* Working Directory\n%s\n\n" work-dir))
      (insert "* Scripts\n")
      (insert (format "[[file:%s]]\n\n" script-path))
      (insert "* Figures\n")
      (insert (format "#+ATTR_HTML: :width %d\n" width))
      (insert "#+ATTR_LATEX: :float t :placement [H]\n")
      (insert (format "[[file:%s]]\n\n" image-path)))
    (let ((buf (find-file-noselect org-file)))
      (with-current-buffer buf
        (let ((org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
              (org-latex-image-default-width "0.8\\linewidth"))
          (org-latex-export-to-pdf))
        (when (file-exists-p pdf-file)
          (goto-char (point-max))
          (insert "\n* PDF\n")
          (insert (format "[[file:%s]]\n\n" pdf-file))
          (save-buffer)
          (revert-buffer t t)
          (org-inline-anim-mode 1)
          (org-display-inline-images)
          (let ((buffer-save-without-query t))
              (save-buffer))
          (revert-buffer t t)))
      (pop-to-buffer buf))))