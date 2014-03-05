;;; rcirc-controls.el --- control sequences
;; Copyright 2008  Alex Schroeder

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Code:

(require 'rcirc)

(defvar rcirc-color-vector ["black" "red" "green" "yellow" "blue"
			    "magenta" "cyan" "white"]
  "Vector of color names for the numbers 0-7.")

(defun rcirc-markup-colors (&rest ignore)
  (while (re-search-forward "\C-c\\(0?[0-7]\\)\\(.*?\\)\C-c" nil t)
    (rcirc-add-face (match-beginning 0) (match-end 0)
		    (cons 'foreground-color
			  (aref rcirc-color-vector
				(string-to-number (match-string 1)))))
    ;; start deleting at the end
    (delete-region (1- (match-end 0)) (match-end 0))
    (delete-region (match-beginning 0) (match-end 1))))

(add-hook 'rcirc-markup-text-functions 'rcirc-markup-colors)

(defvar rcirc-control-alist '(("\C-b" . bold) ("\C-_" . underline)
			      ("\C-v" . inverse))
  "Alist of control sequences and faces to use.")

(defun rcirc-markup-controls (&rest ignore)
  (dolist (item rcirc-control-alist)
    (while (re-search-forward (concat (car item) "\\(.*?\\)" (car item)) nil t)
      (replace-match (match-string 1))
      (rcirc-add-face (match-beginning 0) (match-end 0) (cdr item)))))

(add-hook 'rcirc-markup-text-functions 'rcirc-markup-controls)

(unless (facep 'inverse)
  (make-face 'inverse)
  (set-face-inverse-video-p 'inverse t))

(provide 'rcirc-controls)

;;; rcirc-controls.el ends here
