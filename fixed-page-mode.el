;;; fixed-page-mode.el --- A fixed page length mode  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2023 Igor Wojnicki

;; Author: Igor Wojnicki <wojnicki@gmail.com>
;; Version: 1.0
;; Keywords: text, page

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Fixed Page mode is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; Fixed Page mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; A page-based text editing/note taking/concept thinking Emacs minor
;;; mode.  Presents buffer's content as pages of predefined number of
;;; lines (50 by default).  It is an analog of pages in a notebook. It
;;; can be used with org mode or any other text mode.

;;; Code:

(defgroup fixed-page nil
  "A fixed page length mode and page-by-page navigation."
  :package-version '(fixed-page . "1.0")
  :group 'editing
  :prefix "fixed-page")

(defcustom fixed-page-length 50
  "Page length, in lines"
  :type 'integer)

(make-variable-buffer-local 'fixed-page-length)

(defvar-local fixed-page-number-mode-line " FP"
   "Modeline page number indicator")

(defvar org-element-use-cache)

(defun fixed-page-number ()
  "Returns a page number, counting from 0."
  (/ (1- (line-number-at-pos)) fixed-page-length))

(defun fixed-page-count-lines ()
  "Count lines on the current page."
  (if (buffer-narrowed-p) ;; ugly hack, if there is no narrowing report fixed-page-length
      (count-lines (point-min) (point-max))
    fixed-page-length))

(defun fixed-page-next (&optional reverse)
  "Jumps to next page.
If reverse is not null jumps to a previous page.
Page length is defined by fixed-page-length variable."
  (interactive)
  (setq reverse (if reverse -1 1))
  (widen)
  (forward-line (* reverse fixed-page-length))
  (fixed-page-narrow))

(defun fixed-page-prev ()
  "Jumps to previous page."
  (interactive)
  (fixed-page-next 1))

(defun fixed-page-goto-page (page-number)
  "Jump to the given page."
  (interactive "nGo to page number:")
  (widen)
  (goto-char 1)
  (forward-line (* page-number fixed-page-length))
  (fixed-page-narrow))

(defun fixed-page--length-compensate ()
  "Check if current page is of fixed-page-length.
If not add newlines at the end."
  (let ((lines-to-add (- fixed-page-length (fixed-page-count-lines))))
    (when lines-to-add
      (save-excursion
	(goto-char (point-max))
	(newline lines-to-add)
	lines-to-add))))

(defun fixed-page-mode-empty-lines-at-end ()
  "Count empty lines at the end of buffer."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward "\n[:space:]")
    (count-lines (point) (point-max))))

(defun fixed-page-narrow ()
    "Narrow to current page based on fixed-page-length variable."
    (save-excursion
      (let ((fixed-page-number-value (fixed-page-number)))
      ;; Go to a begining of current page.
	(goto-char (point-min))
	(forward-line (* fixed-page-number-value fixed-page-length))
	(setq fixed-page-number-mode-line (format " FP(%d)" fixed-page-number-value)))
      ;; Narrow
      (narrow-to-region (point)
			(progn
			  (forward-line fixed-page-length)
			  (point))))
    (goto-char (point-min)))

(defun fixed-page-mode-remove-lines-from-end (lines)
  "Remove lines from the end of buffer."
  (save-excursion
    (goto-char (point-max))
    (forward-line (- lines))
    (delete-region (point) (point-max))))

(defun fixed-page--mode-prevent-too-long-page (beg end _len)
  "after-change-functions hook to control page length."
  (when (not undo-in-progress)
    (let* ((lines (fixed-page-count-lines)) 
	   (stuff-lines (- lines (1- (fixed-page-mode-empty-lines-at-end))))) ;; # lines that are not empty at the end
      (if (<= lines fixed-page-length)
	  (fixed-page--length-compensate)
	(if (<= stuff-lines fixed-page-length)
	    (fixed-page-mode-remove-lines-from-end (- lines fixed-page-length))
	  (kill-region beg end))))))

;;;###autoload
(define-minor-mode fixed-page-mode
  "A page-based text editing/note taking/concept thinking.
Edit text page by page."
  :lighter fixed-page-number-mode-line
  :group 'fixed-page
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<next>") 'fixed-page-next)
            (define-key map (kbd "<prior>") 'fixed-page-prev)	    
            map)
  (if (bound-and-true-p fixed-page-mode)
      (progn
	(when (require 'org nil 'noerror)
	  (setq org-element-use-cache nil)) ;; WORKAROUND with org mode cache, otherwise org mode reports warnings
	(add-hook 'after-change-functions #'fixed-page--mode-prevent-too-long-page 0 1)
	(fixed-page-narrow))
    (remove-hook 'after-change-functions #'fixed-page--mode-prevent-too-long-page 1)
    (widen)))

(provide 'fixed-page-mode)

;;; fixed-page-mode.el ends here
