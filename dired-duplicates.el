;;; dired-duplicates.el --- find duplicate files using dired

;; Copyright (C) 2014 Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Version: 1
;; Keywords: dired

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Within `dired', mark the files you want to look for and use M-x
;; dired-find-duplicates. This will ask you about the target directory
;; and it will recursively search for matches.
;;
;; 1. The name must match.
;; 2. The size must match.
;; 3. Depending on how you called it, the last modified timestamp must match.
;; 4. Or alternatively an MD5 hash is computed and compared.
;;
;; The marked files failing these tests are shown in a new dired
;; buffer called "*duplicated files*".

;;; Code:

(defun dired-find-duplicates (files dir &optional ts nodir)
  "Find duplicates of files and put them in a dired buffer.
FILES is a list of files which will be compared. DIR is the
directory which will be checked for duplicates of any of the
files in the list.  Any matching files will be placed in a new
dired buffer with name *duplicated files*.

When called interactively from a dired buffer, the marked files
in that dired buffer will be treated as the orginals whose
duplicates are to be found, and the user will be prompted for a
directory to search for duplicates.

Optional argument TS indicates that you want to compare
timestamps instead of computing a costly MD5 hash of the file
content.

Optional argument NODIR indicates that you want to ignore the
directory structure when comparing files. This assumes unique
filenames, obviously."
  (interactive (list (dired-get-marked-files t)
		     (read-directory-name "Directory to be checked: ")
		     (y-or-n-p "Save time and use timestamp instead of MD5 hash? ")
		     (y-or-n-p "Ignore directory structures? ")))
  (let ((candidates (directory-files dir))
	different-files)
    ;; recurse subdirectories for both lists
    (dolist (file files)
      (when (and (file-directory-p file)
		 (not (string= (file-name-nondirectory file) "."))
		 (not (string= (file-name-nondirectory file) "..")))
	(setq files (nconc files (directory-files dir)))))
    (dolist (file candidates)
      (when (and (file-directory-p file)
		 (not (string= (file-name-nondirectory file) "."))
		 (not (string= (file-name-nondirectory file) "..")))
	(setq candidates (nconc candidates (directory-files dir)))))
    ;; go through the files
    (dolist (file files)
      (message "Looking at %s..." file)
      (let* ((other (if nodir
			(cdr (assoc (file-name-nondirectory file)
				    (mapcar (lambda (f)
					      (cons (file-name-nondirectory f)
						    (expand-file-name f dir)))
					    candidates)))
		      (expand-file-name file dir)))
	     (file-attributes (file-attributes file))
	     (other-attributes (and other (file-attributes other))))
	(cond ((not file-attributes)
	       (message "%s cannot be read" file)
	       (push file different-files))
	      ((not other-attributes)
	       (message "%s is not in the checked directory" file)
	       (push file different-files))
	      ((not (equal (nth 7 file-attributes)
			   (nth 7 other-attributes)))
	       (message "%s doesn't have the same size" file)
	       (push file different-files))
	      ((and ts
		    (not (equal (nth 5 file-attributes)
				(nth 5 other-attributes))))
	       (message "%s doesn't have the same timestamp" file)
	       (push file different-files))
	      ((and (not ts)
		    (not (string= (with-temp-buffer
				    (insert-file-literally file)
				    (md5 (current-buffer)))
				  (with-temp-buffer
				    (insert-file-literally other)
				    (md5 (current-buffer))))))
	       (message "%s doesn't have the same content (MD5 hash differs)"
			file)
	       (push file different-files)))))
    (with-current-buffer (get-buffer-create "*duplicated files*")
      (erase-buffer))
    (dired (cons "*duplicated files*" (reverse different-files)))))

(provide 'dired-duplicates)

;;; dired-duplicates.el ends here
