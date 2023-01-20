# carve.el

emacs integration with borkdude/carve

## Installation

Copy to your elpa directory and then `(require carve)` from you `init.el`.

## Usage

Run `M-x carve-project` (`C-c C-c p`) to run against the whole project or `M-x carve-ns` (`C-c C-c n`) for just the current namespace.

The output window will show the carve results, with which you can:
- `Enter`, or click, to navigate to the declaration
- `g` to refresh the output
- `i` to add symbol at point to your `.carve/ignore` file
- `q` to quit
- (Coming soon) Press `d` to delete the form

Feedback and PRs gratefully accepted.
