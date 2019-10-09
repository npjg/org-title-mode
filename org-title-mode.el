;;; org-title-mode --- keep your org buffers semantically named

;; Licensed under the same terms as Emacs and under the MIT license.

;; Author: Nathanael Gentry <ngentry1@liberty.edu>
;; URL: http://github.com/npjg/org-title-mode
;; Package-Requires:
;; Version: 0.1
;; Created: 2019-10-07
;; By: Nathanael Gentry <ngentry1@liberty.edu>
;; Keywords:

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
      (org-title--set-buffer-name (current-buffer))
    (org-title--unset-buffer-name (current-buffer))))

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

(defun org-keyword-alist (buffer-or-name)
  "Parse the Org buffer BUFFER-OR-NAME and return a cons list of (property . value)
from property lines, e.g. lines that have #+PROPERTY: value."
  (with-current-buffer buffer-or-name
    (org-element-map (org-element-parse-buffer 'element) 'keyword
      (lambda (keyword) (cons (org-element-property :key keyword)
                              (org-element-property :value keyword))))))

(defun org-keyword-alist-get (keyword buffer-or-name)
  "get the value of a KEYWORD in the form of #+KEYWORD: value"
  (cdr (assoc keyword (org-keyword-alist buffer-or-name))))

;;; Mode Internals

(defun org-title--set-buffer-name (&optional buffer-or-name)
  "Get the title of the buffer, and use it to set the buffer's
name."
  (interactive)
  (let* ((buffer-or-name (or buffer-or-name (current-buffer)))
         (buffer-name (org-keyword-alist-get "TITLE" buffer-or-name)))
    (with-current-buffer buffer-or-name
      (rename-buffer buffer-name))))

(defun org-title--unset-buffer-name (&optional buffer-or-name)
  (interactive)
  (let* ((buffer-or-name (or buffer-or-name (current-buffer)))
         (buffer-name (org-keyword-alist-get "TITLE" buffer-or-name)))
    (with-current-buffer buffer-or-name
      (rename-buffer (relative-path (buffer-file-name))))))
