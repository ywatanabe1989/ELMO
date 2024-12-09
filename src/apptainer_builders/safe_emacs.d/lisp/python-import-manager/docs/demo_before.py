#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Time-stamp: "2024-11-01 02:46:13 (ywatanabe)"
# File: ./python-import-manager/docs/demo_before.py

import numpy as np # Duplicated
import numpy as np # Duplicated
from numpy import array
import os # Missed placement
import sys # Missed placement
from typing import List, Dict, Optional # Unused
import seaborn as sns # Unused
from pathlib import Path


def main():
    arr = np.zeros((10, 10))

    df = pd.DataFrame(arr) # Missed, renamed package

    current_dir = os.getcwd()


    utils.my_awesome_func() # Custom module, Missed

    # Missed import, even from the second-level module
    plt.figure()
    plt.imshow(data)
    plt.show()

    # Missing import (collections)
    counter = Counter([1, 2, 2, 3, 3, 3])
    print(counter)

if __name__ == '__main__':
    main()


# EOF
