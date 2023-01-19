# carve.el

emacs integration with borkdude/carve

## Installation

Copy to your elpa directory and then `(require carve)`

## Usage

Run `M-x carve-project` to run against the whole project or `M-x carve-ns` for just the current namespace.

The output window will show the carve results, with which you can:
- Press `Enter`, or click, to navigate to the declaration
- Press `q` to quit
- (Coming soon) Press `i` to add to you `.carve/ignore` file
- (Coming soon) Press `d` to delete the form

Feedback and PRs gratefully accepted.
