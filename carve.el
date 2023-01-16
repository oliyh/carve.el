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

;; Command to run carve

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

(defun carve ()
  (let* ((buf (get-buffer-create "*carve-output*"))
         (inhibit-read-only t)
         (current-file-directory (file-name-directory buffer-file-name))
         (project-root (projectile-project-root))
         (has-config (file-exists-p (concat project-root ".carve/config.edn")))
         (carve-opts (if has-config
                         (list "--opts")
                       (list "--opts" "{:paths [\"src\" \"test\"] :report {:format :text}}"))))
    (with-current-buffer buf
      (erase-buffer)
      (grep-mode) ;; enables file linking, q for quit
      (setq default-directory project-root) ;; run carve process in correct directory
      (select-window (display-buffer buf))
      ;; todo make this a minor mode if need it back
      ;;      (carve-mode)
      (print (concat  "Running: " (string-join (cons carve-command carve-opts) " ")))
      (let ((process (apply 'start-file-process "carve" buf carve-command carve-opts)))
        (while (accept-process-output process)))
      (goto-char (point-min)))))

(global-set-key (kbd "C-c c") (lambda () (interactive) (carve)))

;; (carve)

;; todo
;; - check for existence of src / test and use them if present
;; - find project root to execute from (projectile? or something else?)
;; - carve-current-file: set carve path to current file

;; - commands could be carve-current-file, carve-project
;; - keyboard shortcuts, example at least
;; - key to add line to ignore file

;; - in result buffer, could have a key which will delete the form it refers to?


;; (carve)
;;; carve.el ends here
