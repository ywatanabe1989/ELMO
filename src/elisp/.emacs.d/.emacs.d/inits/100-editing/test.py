#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Time-stamp: "ywatanabe (2024-11-09 12:34:13)"
# File: ./.dotfiles/.emacs.d/inits/100-editing/test.py

"""
1. Functionality:
   - (e.g., Executes XYZ operation)
2. Input:
   - (e.g., Required data for XYZ)
3. Output:
   - (e.g., Results of XYZ operation)
4. Prerequisites:
   - (e.g., Necessary dependencies for XYZ)

(Remove me: Please fill docstrings above, while keeping the bulette point style, and remove this instruction line)
"""

"""Imports"""
import sys
import matplotlib.pyplot as plt
import mngs
import argparse
    
"""Warnings"""
# mngs.pd.ignore_SettingWithCopyWarning()
# warnings.simplefilter("ignore", UserWarning)
# with warnings.catch_warnings():
#     warnings.simplefilter("ignore", UserWarning)

"""Parameters"""
# from mngs.io import load_configs
# CONFIG = load_configs()

"""Functions & Classes"""


def main():
    mngs.str.printc("aaa")
    pass

def arg_parse() -> argparse.Namespace:
    """Parse command line arguments."""
    global args
    script_mode = mngs.gen.is_script()
    parser = argparse.ArgumentParser(description='')
    parser.add_argument('--var', '-v', type=int, choices=None, default=1, help='(default: %%(default)s)')
    # parser.add_argument('--flag', '-f', action='store_true', default=False, help='(default: %%(default)s)')
    args = parser.parse_args()
    mngs.str.printc(args, c='yellow')
    return args
    
def run_main() -> None:
    """Initialize mngs framework, run main function, and cleanup."""
    global CONFIG, CC, sys, plt

    # Configurations and starting logging    
    CONFIG, sys.stdout, sys.stderr, plt, CC = mngs.gen.start(
        sys,
        plt,
        verbose=False,
        agg=True,
        # sdir_suffix="",
    )

    args = arg_parse()

    # Main
    exit_status = main()

    # Cleanup
    mngs.gen.close(
        CONFIG,
        verbose=False,
        notify=False,
        message="",
        exit_status=exit_status,
    )

if __name__ == "__main__":
    run_main()

# EOF
