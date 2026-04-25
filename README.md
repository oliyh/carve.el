# carve.el

emacs integration with [borkdude/carve](https://github.com/borkdude/carve)

It lets you run carve with a simple key binding and opens a result buffer where you can navigate to the
results and add them to your ignore file with a single keystroke.

## Installation

For project detection, `carve.el` defaults to `projectile` if available,
falling back to Emacs's built-in `project.el`. To force using `project.el`,
even if projectile is installed, set `carve-project-backend` to `'project`.

Download `carve.el` and load it in your emacs `init.el`:

```elisp
(load "~/path/to/carve.el")
```

## Usage

Run `M-x carve-project` (`C-c C-c p`) to run against the whole project or `M-x carve-ns` (`C-c C-c n`) for just the current namespace.

The output window will show the carve results, with which you can:
- `Enter`, or click, to navigate to the declaration
- `g` to refresh the output
- `i` to add symbol at point to your `.carve/ignore` file
- `q` to quit
- (Coming soon) Press `d` to delete the form

Feedback and PRs gratefully accepted.
