;;; org-title-mode --- keep your org buffers semantically named

;; Licensed under the same terms as Emacs and under the MIT license.

;; Author: Nathanael Gentry <ngentry1@liberty.edu>
;; URL: http://github.com/npjg/org-title-mode
;; Package-Requires:
;; Version: 0.1
;; Created: 2019-10-08
;; By: Nathanael Gentry <ngentry1@liberty.edu>
;; Keywords: org, buffers

;;; Commentary:

;; This package provides semantic naming for Org buffers. `org-title-mode', its
;; namesake minor mode, sets the name of an Org buffer to the title of the Org
;; document, with appropriate conflict resolution policies in place.

(require 'org)

;;;###autoload
(define-minor-mode org-title-mode
  "Toggle org-title-mode.
When active in an Org buffer, synchronizes the buffer name with
the title of the Org document."
  :lighter " T "
  (unless (eq (buffer-mode) 'org-mode)
    (user-error "Org-title-mode requires Org mode"))
  (if org-title-mode
      (progn
        (setq-local org-title--original-buffer-name (buffer-name))
        (add-hook 'org-ctrl-c-ctrl-c-final-hook
                  #'org-title--set-buffer-name
                  nil nil)
        (advice-add 'org-refresh-category-properties :override
                    #'org-title-refresh-category-properties)
        (org-refresh-category-properties)
        (org-title--set-buffer-name (current-buffer)))
    (remove-hook 'org-ctrl-c-ctrl-c-final-hook
                 #'org-title--set-buffer-name
                 nil)
    (advice-remove 'org-refresh-category-properties
                   #'org-title-refresh-category-properties)
    (org-refresh-category-properties)
    (org-title--unset-buffer-name (current-buffer))
    (kill-local-variable 'org-title--original-buffer-name)))

(defun org-title-refresh-category-properties ()
  "Refresh category text properties in the buffe, replacing
default file-name category with the title category."
  (let ((org-category (org-keyword-alist-get "TITLE")))
    (ad-with-originals 'org-refresh-category-properties
      (org-refresh-category-properties))))

;;; Utility Functions

(defun split-file-name (file &optional always-parse-as-file)
  "Takes a file name FILE, and returns a list whose `car' is a
relative file name and whose `cdr' is a directory name. If FILE
is actually a directory name, the `cdr' will be the empty string;
set `always-parse-as-file' true to convert FILE to a directory
file name before parsing it. View this function as the inverse of
`expand-file-name'."
  (let* ((fixed
          (if always-parse-as-file
              file
            (directory-file-name file)))
         (parent (file-name-directory fixed)))
    (cons parent (directory-file-name (string-remove-prefix parent file)))))

(defun relative-path (file)
  "Takes a file name FILE, and strips away all path information."
  (cdr (split-file-name file t)))

(defun buffer-mode (&optional buffer-or-name)
  "Return the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
   (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

(defun org-keyword-alist-get (keyword &optional buffer-or-name)
  "Get the value from a #+KEYWORD: value line in
BUFFER-OR-NAME or the current buffer. Return nil if there is
none.

Adapted from John Kitchin, \"Getting keyword options in org-files.\""
  (interactive (list (read-string "Keyword: ")))
  (with-current-buffer (or buffer-or-name (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t)
            (re (format "^#\\+%s:[ \t]+\\([^\t\n]+\\)" keyword)))
        (if (save-excursion
              (or (re-search-forward re nil t)
                  (re-search-backward re nil t)))
            (match-string-no-properties 1))))))

;;; Mode Internals

;;;; Name Setter

(defun org-title--set-buffer-name (&optional buffer-or-name)
  "Get the title of the buffer, and use it to set the buffer's
name."
  (interactive)
  (let* ((buffer-or-name (or buffer-or-name (current-buffer)))
         (buffer-name (org-title--format-semantic-name buffer-or-name)))
    (when buffer-name
      (with-current-buffer buffer-or-name
        (rename-buffer buffer-name)))))

(defun org-title--format-semantic-name (buffer-or-name)
  "Return the semantic buffer name, or nil if there is not enough
information to do so."
  (let ((semantic-name (org-keyword-alist-get "TITLE" buffer-or-name)))
    (when (and semantic-name (> (length semantic-name) 0)
               (not (equal (buffer-name) semantic-name)))
      (org-title--resolve-buffer-name-conflict semantic-name))))

(defun org-title--resolve-buffer-name-conflict (buffer-name-maybe)
  (if (not (get-buffer buffer-name-maybe))
      buffer-name-maybe
    (org-title--resolve-buffer-name-conflict
     ;; a silly heuristic: just wrap in angle braces until it's unique
     ;; perhaps a better one: (generate-new-buffer-name buffer-name-maybe)
     (concat "<" buffer-name-maybe ">"))))

;;;; Name Unsetter

(defun org-title--unset-buffer-name (&optional buffer-or-name)
  (interactive)
  (let* ((buffer-or-name (or buffer-or-name (current-buffer)))
         (buffer-name (org-keyword-alist-get "TITLE" buffer-or-name)))
    (org-title--format-revert-name buffer-or-name)))

(defun org-title--format-revert-name (buffer-or-name)
  (with-current-buffer buffer-or-name
    (rename-buffer org-title--original-buffer-name)))

(provide 'org-title-mode)

;;; org-title-mode.el ends here
