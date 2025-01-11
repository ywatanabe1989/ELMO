<!-- ---
!-- Timestamp: 2025-01-11 18:14:42
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/docs/installation/mermaid.md
!-- --- -->
<!-- ---
!-- title: 2025-01-02 18:57:59
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/docs/installation/mermaid.md
!-- --- -->

Below is an example Emacs configuration snippet for mermaid-mode and ob-mermaid:

```elisp
;;; -*- lexical-binding: t -*-
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/inits/200-interface/050-mermaid.el

(defun my/mermaid-compile ()
  "Generate PNG, SVG, and GIF after saving Mermaid files."
  (when (and (eq major-mode 'mermaid-mode) (buffer-file-name))
    (let* ((input-file (buffer-file-name))
           (base-name (file-name-sans-extension input-file)))
      (call-process "mmdc" nil nil nil
                    "-i" input-file
                    "-o" (concat base-name ".png")
                    "--backgroundColor" "transparent")
      (call-process "mmdc" nil nil nil
                    "-i" input-file
                    "-o" (concat base-name ".svg")
                    "--backgroundColor" "transparent"
                    "-f" "svg")
      (call-process "convert" nil nil nil
                    (concat base-name ".png")
                    (concat base-name ".gif"))
      (message "Created %s.png, %s.svg, and %s.gif" base-name base-name base-name))))

(defun my/mermaid-after-save-hook ()
  (my/mermaid-compile))

(use-package mermaid-mode
  :ensure t
  :custom
  (setq mermaid-output-format ".png")
  :hook
  (after-save . my/mermaid-after-save-hook))

(use-package ob-mermaid
  :ensure t
  :after org
  :custom
  (ob-mermaid-cli-path "/usr/local/bin/mmdc")
  (ob-mermaid-extra-args "--backgroundColor transparent -f svg")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t))))

```

### System Requirements
1. **Node.js >= 18**  
2. **mermaid-cli** (installed globally via npm)  
3. **chromium-browser** or equivalent  
4. **ImageMagick** (for convert)  

Quick test:

```bash
echo "graph TD; A-->B;" > /tmp/test.mmd
mmdc -i /tmp/test.mmd -o /tmp/test.png
```

If successful, you should see `/tmp/test.png` generated.
<!-- <\!-- ---
 !-- !-- title: 2025-01-02 18:57:59
 !-- !-- author: Yusuke Watanabe
 !-- !-- date: /home/ywatanabe/proj/llemacs/docs/installation/mermaid.md
 !-- !-- --- -\->
 !-- 
 !-- ``` elisp
 !-- ;;; -*- lexical-binding: t -*-
 !-- ;;; Author: 2025-01-02 18:57:16
 !-- ;;; Time-stamp: <2025-01-02 18:57:59 (ywatanabe)>
 !-- ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/inits/200-interface/050-mermaid.el
 !-- 
 !-- (defun my/mermaid-compile ()
 !--   "Generate PNG, SVG, and GIF after saving Mermaid files."
 !--   (interactive)
 !--   (when (and (eq major-mode 'mermaid-mode)
 !--              (buffer-file-name))
 !--     (let* ((input-file (buffer-file-name))
 !--            (base-name (file-name-sans-extension input-file)))
 !--       (call-process "mmdc" nil nil nil
 !--                     "-i" input-file
 !--                     "-o" (concat base-name ".png")
 !--                     "--backgroundColor" "transparent")
 !--       (call-process "mmdc" nil nil nil
 !--                     "-i" input-file
 !--                     "-o" (concat base-name ".svg")
 !--                     "--backgroundColor" "transparent"
 !--                     "-f" "svg")
 !--       (call-process "convert" nil nil nil
 !--                     (concat base-name ".png")
 !--                     (concat base-name ".gif"))
 !--       (message "Created %s.png, %s.svg and %s.gif" base-name base-name base-name))))
 !-- 
 !-- (defun my/mermaid-after-save-hook ()
 !--   (my/mermaid-compile))
 !-- 
 !-- (use-package mermaid-mode
 !--   :ensure t
 !--   :custom
 !--   (setq mermaid-output-format ".png")
 !--   :hook
 !--   (after-save . my/mermaid-after-save-hook))
 !-- 
 !-- (use-package ob-mermaid
 !--   :ensure t
 !--   :custom
 !--   (ob-mermaid-cli-path "/usr/local/bin/mmdc")
 !--   (ob-mermaid-extra-args "--backgroundColor transparent -f svg"))
 !-- 
 !-- (org-babel-do-load-languages
 !--  'org-babel-load-languages
 !--  '((mermaid . t)))
 !-- 
 !-- (message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
 !-- 
 !-- ;; # 1. Node.js 18+ Setup
 !-- ;; sudo apt-get remove nodejs libnode-dev
 !-- ;; sudo apt-get autoremove
 !-- ;; sudo apt-get install nodejs -y
 !-- 
 !-- ;; # 2. Mermaid CLI & Dependencies
 !-- ;; sudo npm install -g @mermaid-js/mermaid-cli
 !-- ;; sudo apt-get install -y chromium-browser
 !-- ;; sudo npm install -g puppeteer
 !-- ;; npx puppeteer browsers install chrome-headless-shell
 !-- 
 !-- ;; # 3. Test Setup
 !-- ;; echo "graph TD; A-\->B;" > /tmp/test.mmd
 !-- ;; mmdc -i /tmp/test.mmd -o test.png
 !-- 
 !-- (message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
 !-- ``` -->
