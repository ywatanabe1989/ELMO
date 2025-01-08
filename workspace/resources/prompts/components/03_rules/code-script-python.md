<!-- ---
!-- Timestamp: 2025-01-09 06:31:59
!-- Author: ywatanabe
!-- File: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/code-script-python.md
!-- --- -->

# Rule: code-script-python
* Use Python binary at `llemacs--path-python-sys` (= `(exapand-file-name "bin/python" llemacs--path-python-env-sys`)
* Ensure to use `argparse` to run Python scripts from Elisp.
* Save data to `llemacs--path-pj-data` with appropriate directory structure.
* Save Python script files under `(expand-file-name "python" llemacs--path-pj-scripts)`.
* Produced results from python script should be saved under `llemacs--path-pj-results`.
* GPU may not be available
* For ML tasks, please use sklearn, pytorch, and huggingface
* The following packages are installed in the environment:
GitPython
Pillow
PyYAML
Pyarrow
accelerate
aiohttp
ansi-escapes
anthropic
black
bs4
catboost
chardet
einops
epc
flake8
flask
geom_median
google-genai
googlesearch-python
h5py
html2text
icecream
ipdb
ipython<8.0.0
isort
jedi
joblib
joypy
julius
lxml
lxml_html_clean
markdown
markdown2
matplotlib
mne
more-itertools
natsort
numpy
obspy
openai
openpyxl
optuna
pandas
pandas
plotly
plyer
psutil
pybids
pyedflib
pyls
pymatreader
pyperclip
pyright
pytest
pytest-cov
pytest-env
pytest-xdist
python-docx
python-lsp-server
pytorch_pretrained_vit
readability
readability-lxml
readchar
reportlab
requests
ripple_detection
ruamel.yaml
ruff-lsp
scikit-image
scikit-learn
scipy
seaborn
semgrep
setuptools
six
sktime
sounddevice
statsmodels
sympy
tabulate
tensorpac
termplotlib
tk
tldr
transformers
twine
umap-learn
wheel
xarray
xlrd
xmltodict
torchsummary
torch
torchvision
torchaudio
psycopg2-binary>=2.9.9  # Changed from psycopg2
sqlalchemy>=2.0.0
groq
