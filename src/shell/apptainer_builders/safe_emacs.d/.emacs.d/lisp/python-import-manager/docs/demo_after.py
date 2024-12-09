#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Time-stamp: "2024-11-01 02:48:50 (ywatanabe)"
# File: ./python-import-manager/docs/demo_after.py

import os  # Missed placement
from typing import Optional  # Unused

import matplotlib.pyplot as plt
import numpy as np  # Duplicated
import pandas as pd
import scripts.utils as utils
from counter import Counter
from data import data


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

# (pim-auto-mode)


# EOF
