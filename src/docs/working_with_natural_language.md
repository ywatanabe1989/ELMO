<!-- ---
!-- title: ./self-evolving-agent/src/docs/working_with_natural_language.md
!-- author: ywatanabe
!-- date: 2024-12-05 23:19:12
!-- --- -->


# Working with Natural Language

``` elisp
# Fixme: `../src/sea-server.el` should control server setup
./src/sea_server.sh kill
./src/sea_server.sh start

(sea-run "open a new buffer and show welcome message")

(sea-run "
Write code to calculate DMD from EEG demo signal
Plot and show figures
")

(sea-run "
Write code to calculate intersting feature from EEG demo signal
Plot and show figures
")


(defun test () 
(interactive)
(sea-run "
Write code to calculate DMD from EEG demo signal
Plot and show figures
")
)

```
