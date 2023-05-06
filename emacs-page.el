;; A page-based text editing/note taking/concept thinking.
;; acts funny with org mode and M-RET

(make-variable-buffer-local
 (defvar page-length 24
       "Page length, in lines"))


(make-variable-buffer-local
 (defvar page-number-modeline ""
       "Modeline page number indicator"))


(defun page-number ()
  "Returns a page number, counting from 0."
  (/ (1- (line-number-at-pos)) page-length))


(defun page-next (&optional reverse)
  "Jumps to next page.
If reverse is not null jumps to a previous page.
Page length is defined by page-length variable."
  (interactive)
  (setq reverse (if reverse -1 1))
  (widen)
  (forward-line (* reverse page-length))
  (page-narrow))


(defun page-prev ()
  "Jumps to previous page."
  (interactive)
  (page-next 1))


(defun page-length-compensate ()
  "Check if current page is of page-length.
If not add newlines at the end."
  (let ((lines-to-add (- page-length (count-lines (point-min) (point-max)))))
    (message "lines missing: %d" lines-to-add)
    (when lines-to-add
      (save-excursion
	(end-of-buffer)
	(newline lines-to-add)
	lines-to-add))))


(defun page-mode-empty-lines-at-end ()
  "Count empty lines at the end of buffer."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward "\n[:space:]")
    (count-lines (point) (point-max))))


(defun page-narrow ()
    "Narrow to current page based on page-length variable."
    (save-excursion
      (let ((page-number-value (page-number)))
      ;; Go to a begining of current page.
	(goto-char (point-min))
	(forward-line (* page-number-value page-length))
	(setq page-number-modeline (format "Pg: %d" page-number-value)))
      ;; Narrow
      (narrow-to-region (point)
			(progn
			  (forward-line page-length)
			  (point)))))


(defun page-mode-remove-lines-from-end (lines)
  "Remove lines from the end of buffer."
  (save-excursion
    (goto-char (point-max))
    (forward-line (- lines))
    (delete-region (point) (point-max))))

;; there is a problem with org-mode cache that is not being updated properly
;; it can be suppressed with:
;;(setq warning-suppress-types (append warning-suppress-types '((org-element-cache))))
;; or cache can be disable with
;;(setq org-element-use-cache nil)
;; but it does not solve the problem
;; FIXME: change to a before-change-functions. hook - it might help
;; example: https://www.emacswiki.org/emacs/ChangeHook
;; or maybe use just (undo) - bad idea, uno/redo loop
(defun page-mode-prevent-too-long-page (beg end len)
  "after-change-functions hook to control page length."
  (message "%d %d %d" beg end len)
  (when (not undo-in-progress)
    (let* ((lines (count-lines (point-min) (point-max)))
	   (stuff-lines (- lines (1- (page-mode-empty-lines-at-end)))))
      (if (<= lines page-length)
	  (page-length-compensate)
	(if (<= stuff-lines page-length)
	    (page-mode-remove-lines-from-end (- lines page-length))
	  (message "Edit violates maximum page length of %d." page-length)
	  (kill-region beg end)
	  )))
;;    (org-element--cache-sync (current-buffer))
    ))



(define-minor-mode page-mode
  "A page-based text editing/note taking/concept thinking.
Edit text page by page."
  :lighter " Pg"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<next>") 'page-next)
            (define-key map (kbd "<prior>") 'page-prev)	    
            map)
  (if (bound-and-true-p page-mode)
      (progn
	(add-hook 'after-change-functions #'page-mode-prevent-too-long-page 0 1)
	(setq mode-line-format (append mode-line-format '(page-number-modeline))) ;; FIXME: see how pdf-tools does it
	(page-narrow))
    (remove-hook 'after-change-functions #'page-mode-prevent-too-long-page 1)
    (nbutlast mode-line-format)
    (widen)))


;; mode line change using mode-line-position 
;; (setq mode-line-position (list '(:eval iwtmp) mode-line-position))
;; (setq iwtmp " xy ")
;; and undo:
;; (setq mode-line-position (car (cdr mode-line-position)))

  

(provide 'page-mode)


