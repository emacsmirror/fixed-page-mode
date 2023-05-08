;; A page-based text editing/note taking/concept thinking.

;; defcustom?
(make-variable-buffer-local
 (defvar fixed-page-length 24
       "Page length, in lines"))


(make-variable-buffer-local
 (defvar fixed-page-number-modeline ""
       "Modeline page number indicator"))


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


(defun fixed-page-length-compensate ()
  "Check if current page is of fixed-page-length.
If not add newlines at the end."
  (let ((lines-to-add (- fixed-page-length (fixed-page-count-lines))))
    (when lines-to-add
      (save-excursion
	(end-of-buffer)
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
	(setq fixed-page-number-modeline (format "Pg: %d" fixed-page-number-value)))
      ;; Narrow
      (narrow-to-region (point)
			(progn
			  (forward-line fixed-page-length)
			  (point)))))


(defun fixed-page-mode-remove-lines-from-end (lines)
  "Remove lines from the end of buffer."
  (save-excursion
    (goto-char (point-max))
    (forward-line (- lines))
    (delete-region (point) (point-max))))


(defun fixed-page-mode-prevent-too-long-page (beg end len)
  "after-change-functions hook to control page length."
  (when (not undo-in-progress)
    (let* ((lines (fixed-page-count-lines)) 
	   (stuff-lines (- lines (1- (fixed-page-mode-empty-lines-at-end))))) ;; # lines that are not empty at the end
      (if (<= lines fixed-page-length)
	  (fixed-page-length-compensate)
	(if (<= stuff-lines fixed-page-length)
	    (fixed-page-mode-remove-lines-from-end (- lines fixed-page-length))
	  (kill-region beg end))))))


(define-minor-mode fixed-page-mode
  "A page-based text editing/note taking/concept thinking.
Edit text page by page."
  :lighter " Pg"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<next>") 'fixed-page-next)
            (define-key map (kbd "<prior>") 'fixed-page-prev)	    
            map)
  (if (bound-and-true-p fixed-page-mode)
      (progn
	(setq org-element-use-cache nil) ;; WORKAROUND with org mode cache, otherwise org mode reports warnings
	(add-hook 'after-change-functions #'fixed-page-mode-prevent-too-long-page 0 1)
	(setq mode-line-format (append mode-line-format '(fixed-page-number-modeline))) ;; FIXME: there might be a better way to do this, see how pdf-tools does it?
	(fixed-page-narrow))
    (remove-hook 'after-change-functions #'fixed-page-mode-prevent-too-long-page 1)
    (nbutlast mode-line-format)
    (widen)))


;; mode line change using mode-line-position 
;; (setq mode-line-position (list '(:eval iwtmp) mode-line-position))
;; (setq iwtmp " xy ")
;; and undo:
;; (setq mode-line-position (car (cdr mode-line-position)))

  

(provide 'fixed-page-mode)


