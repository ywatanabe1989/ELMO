<!-- ---
!-- title: 2025-01-06 11:39:05
!-- author: ywata-note-win
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/07_tools/lang-python-custom.md
!-- --- -->

### Input
- Format: Python function calls
- Required parameters: function name, arguments
- Optional parameters: keyword arguments
### Output
- Format: Return values as specified per function
- Location: Python workspace
- Success indicators: No errors raised
### Example
```python
def greet(name, greeting="Hello"):
    return f"{greeting}, {name}!"

# Usage
result = greet("World")  # Returns "Hello, World!"
result = greet("User", greeting="Hi")  # Returns "Hi, User!" 
```