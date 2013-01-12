;;; buftoggle.el --- Toggles between related buffers and files

;; Copyright (C) 2012  Michael Weber

;; Author: Michael Weber <michaelw@foldr.org>
;; Keywords: files, extensions, convenience, c

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; buftoggle is based on eassist.el, but without the dependency on
;; CEDET.
;;
;; In addition, buftoggle allows to configure per-directory search
;; paths for buftoggled files.

;;; Code:
(eval-and-compile (require 'cl))

(defvar buftoggle-pairs-alist
  '(("h" "cpp" "cc" "c")
    ("H" "C" "CPP" "CC")
    ("hpp" "cpp" "cc")
    ("hh" "cc" "cpp")
    ("cpp" "h" "hpp")
    ("c" "h")
    ("C" "H")
    ("cc" "hh" "h" "hpp")))

(defvar buftoggle-search-path-alist
  '()
  "*An alist of \(REGEX . LIST-OF-RELATIVE-PATHS).  The directory
part of the buftoggled file is matched against REGEX, and the
rest of the path after the match is taken as the base directory
for the paths in LIST-OF-RELATIVE-PATHS.")


(defun buftoggle ()
  "*Toggles between related buffers and files.

See `buftoggle-pairs-alist' to define related files,
and `buftoggle-search-path-alist' for paths where related files are searched."
  (interactive)
  (let* ((buffer-name (file-name-sans-extension (buffer-name)))
         (ext (file-name-extension (buffer-file-name)))
         (dirname (file-name-directory (buffer-file-name)))
         (basename (file-name-nondirectory
                    (file-name-sans-extension (buffer-file-name))))
         (pair-exts (assoc-default ext buftoggle-pairs-alist 'string=)))
    (cond (pair-exts
           (or
            (loop for ext in pair-exts
                  for match = (concat buffer-name "." ext)
                  when (bufferp (get-buffer match))
                  return (pop-to-buffer match))

            (let ((match (buftoggle-find-pair basename pair-exts)))
              (when match (find-file match)))
                
            (loop for path in (buftoggle-search-path dirname)
                  for match = (buftoggle-recursive-find-path path basename pair-exts)
                  when match return (find-file match))
             
            (message "There is no corresponding buftoggle file")
            nil))
     (t
      (message "No buftoggle associations for this file")
      nil))))

(defun buftoggle-search-path (base-path)
  (let ((paths '()))
    (dolist (path buftoggle-search-path-alist (nreverse paths))
      (destructuring-bind (regex . rel-paths) path
        (when (string-match regex base-path)
          (dolist (rel-path rel-paths)
            (push (expand-file-name rel-path
                                    (file-name-as-directory
                                     (substring base-path 0 (match-end 0))))
                  paths)))))))

(defun buftoggle-find-pair (base-path exts)
  (loop for ext in exts
        for match = (concat base-path "." ext)
        when (file-exists-p match) return match))

(defun buftoggle-recursive-find-path (path basename exts)
  (or (buftoggle-find-pair (concat (file-name-as-directory path) basename) exts)
      (let* ((list (directory-files path t "^[^.]"))
             (dirs (remove-if-not 'file-directory-p list)))
        (loop for dir in dirs
              for match = (buftoggle-recursive-find-path dir basename exts)
              when match return match))))


(provide 'buftoggle)
;;; buftoggle.el ends here
