#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Time-stamp: "2024-12-27 14:14:42 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/llemacs/workspace/resources/scripts/gemini_call.py

__file__ = "/home/ywatanabe/.emacs.d/lisp/llemacs/workspace/resources/scripts/gemini_call.py"

import argparse
import google.generativeai as genai
import os

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("prompt", help="Prompt text for Gemini")
    parser.add_argument("output", help="Output file path")
    args = parser.parse_args()

    try:
        genai.configure(api_key=os.getenv('GOOGLE_API_KEY'))
        model = genai.GenerativeModel(os.getenv('GOOGLE_ENGINE'))
        response = model.generate_content(args.prompt)

        with open(args.output, 'w') as f:
            f.write(response.text)

    except Exception as e:
        print(str(e))
        exit(1)

if __name__ == "__main__":
    main()
