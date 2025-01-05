#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Time-stamp: "2025-01-05 04:54:17 (ywatanabe)"
# File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/core-claude_call.py

__file__ = "/home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/core-claude_call.py"

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Time-stamp: "2025-01-05 04:49:12 (ywatanabe)"
# File: /home/ywatanabe/proj/llemacs/llemacs.el/03-llemacs-llm/core-claude_call.py

import os
import asyncio
import argparse
from anthropic import AsyncAnthropic

async def process_claude(prompt, output_path):
    client = AsyncAnthropic(api_key=os.environ.get("ANTHROPIC_API_KEY"))
    try:
        message = await client.messages.create(
            max_tokens=8192,
            messages=[{
                "role": "user",
                "content": prompt
            }],
            model="claude-3-sonnet-20240229"
        )
        with open(output_path, 'w') as f:
            f.write(str(message.content))

            print(f"Message type: {type(message)}")
            print(f"Content type: {type(message.content)}")
            print(f"Content: {message.content}")


    except Exception as e:
        print(str(e))
        exit(1)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("prompt", help="Prompt text for Claude")
    parser.add_argument("output", help="Output file path")
    args = parser.parse_args()

    asyncio.run(process_claude(args.prompt, args.output))

if __name__ == "__main__":
    main()
