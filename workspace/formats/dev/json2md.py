#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Time-stamp: "2024-12-19 15:22:07 (ywatanabe)"
# File: ./Ninja/workspace/formats/json2md.py

__file__ = "/home/ywatanabe/.emacs.d/lisp/Ninja/workspace/formats/json2md.py"

#!/usr/bin/env python3
import json
import sys

# ## New line is not handled well
# def json_to_md(obj, level=1):
#     output = []
#     if isinstance(obj, dict):
#         for key, value in obj.items():
#             output.append("")  # Add blank line before each header
#             output.append("#" * level + " " + str(key))
#             if isinstance(value, (dict, list)):
#                 output.append(json_to_md(value, level + 1))
#             else:
#                 output.append("")  # Add blank line before content
#                 output.append(str(value))
#                 output.append("")  # Add blank line after content
#     elif isinstance(obj, list):
#         for item in obj:
#             if isinstance(item, (dict, list)):
#                 output.append(json_to_md(item, level))
#             else:
#                 output.append("* " + str(item))
#     return "\n".join(filter(None, output))  # Remove empty strings


def json_to_md(obj, level=1):
    output = []
    if isinstance(obj, dict):
        for key, value in obj.items():
            if output:  # Add extra newline between sections
                output.append("")
            output.append("#" * level + " " + str(key))
            if isinstance(value, (dict, list)):
                output.append(json_to_md(value, level + 1))
            else:
                output.append(str(value) + "\n")
    elif isinstance(obj, list):
        for item in obj:
            if isinstance(item, (dict, list)):
                output.append(json_to_md(item, level))
            else:
                output.append("* " + str(item))
    return "\n".join(filter(None, output))

def main():
    if len(sys.argv) != 2:
        print("Usage: json_to_md.py <input.json>")
        sys.exit(1)

    lpath = sys.argv[1].replace("/./", "/")
    with open(lpath, "r") as f:
        data = json.load(f)

    print(json_to_md(data))


if __name__ == "__main__":
    main()


"""
python ./Ninja/workspace/formats/json2md.py
python -m workspace.formats.json2md
"""

# EOF
