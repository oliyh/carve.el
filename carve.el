;;; carve.el --- emacs integration with borkdude/carve

;; Copyright © 2023 Oliver Hine
;;
;; Author: Oliver Hine

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; https://github.com/oliyh/carve.el

;;; Code:
(require 'projectile)

(defgroup carve nil
  "Settings for carve"
  :group 'tools)

(defcustom carve-command "carve"
  "*The carve command to run"
  :type 'string
  :group 'carve)

(defun kill-carve-buffer ()
  (interactive)
  (kill-buffer))

(defvar carve-mode-map nil
  "Keymap for `carve-mode'.")

(unless carve-mode-map
  (setq carve-mode-map (make-sparse-keymap))
  (define-key carve-mode-map "q" 'kill-carve-buffer)
  (define-key carve-mode-map "Enter" 'carve-mode-visit))

(defun carve-mode ()
  "Major mode for viewing carve output.
\\{carve-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq minor-mode 'carve-mode
        mode-name "Carve output")
  (use-local-map carve-mode-map))

(defun walk-output ()
  (interactive)
  (let (prop-alist)
    (goto-char (point-min))
    (forward-line 2)
    (while (looking-at "\\([^ ]+\\) \\(.+\\)$")
      (let ((file-path (match-string 1))
	    (carved-symbol (match-string 2)))
        (add-text-properties (line-beginning-position) (line-end-position) (list 'carve-symbol carved-symbol))
	(when (assoc carved-symbol prop-alist)
	  (error "Duplicated property %s" carved-symbol))
	(setq prop-alist (cons (cons file-path carved-symbol) prop-alist)))
      (forward-line 1))
    prop-alist))

(defun run-carve (project-root carve-opts)
  (let ((buf (get-buffer-create "*carve-output*"))
        (inhibit-read-only t)
        (command (string-join (cons carve-command carve-opts) " ")))
    (with-current-buffer buf
      (erase-buffer)
      (setq default-directory project-root) ;; run carve process in correct directory
      (select-window (display-buffer buf))
      ;; todo make this a minor mode if need it back
      ;;      (carve-mode)
      (print (concat "Running: " command))
      (goto-char (point-min))
      (insert command)
      (insert "\n\n")
      (let ((process (apply 'start-file-process "carve" buf carve-command carve-opts)))
        (while (accept-process-output process)))

      (walk-output)
      (grep-mode) ;; enables file linking, q for quit
      (goto-char (point-min))
      (forward-line 2))))

(defun carve-project ()
  (interactive)
  (let* ((current-file-directory (file-name-directory buffer-file-name))
         (project-root (projectile-project-root))
         (has-config (file-exists-p (concat project-root ".carve/config.edn")))
         (carve-opts (if has-config
                         (list "--opts")
                       (list "--opts" "{:paths [\"src\" \"test\"] :report {:format :text}}"))))
    (run-carve project-root carve-opts)))

(defun carve-ns ()
  (interactive)
  (let* ((project-root (projectile-project-root))
         (project-file-path (file-relative-name buffer-file-name project-root))
         (carve-opts (list "--opts" (concat "{:paths [\"" project-file-path "\"] :report {:format :text}}"))))
    (run-carve project-root carve-opts)))

(global-set-key (kbd "C-c C-c p") 'carve-project)
(global-set-key (kbd "C-c C-c n") 'carve-ns)

;; (carve)

;; todo
;; - check for existence of src / test and use them if present

;; - keyboard shortcuts, example at least
;; - key to add line to ignore file

;; - in result buffer, could have a key which will delete the form it refers to?

;;; carve.el ends here
