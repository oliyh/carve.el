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
(defgroup carve nil
  "Settings for carve"
  :group 'tools)

(defcustom carve-command "carve"
  "*The carve command to run"
  :type 'string
  :group 'carve)

(defun carve ()
  (let ((buf (get-buffer-create "*carve-output*"))
        (dir default-directory))
    (with-current-buffer buf
      (erase-buffer)
      (setq default-directory dir)
      (start-file-process "carve" buf carve-command "--opts" "'{:paths [\"src\" \"test\"] :report {:format :text}}'"))))

;; todo
;; - make the command run properly
;; - check for existence of .carve config in project, and use that?
;; - set carve path to current file

;; - make the buffer with the results pop open
;; - make the result buffer read-only

;; - commands could be carve-current-file, carve-project

;; - in result buffer, could have a key which will delete the form it refers to?

;; (carve)
;;; carve.el ends here
