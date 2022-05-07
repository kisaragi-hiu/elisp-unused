;;; elisp-unused.el --- List unused functions in Emacs Lisp -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (projectile "2.5.0") (dumb-jump "0.5.4") (dash "2.19.1") (s "1.12.0"))
;; Homepage: https://github.com/kisaragi-hiu/elisp-unused
;; Keywords: lisp


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; I haven't been able to find a command that lists all unused
;; functions in an Emacs Lisp project, so here's my attempt at it.

;; The main concept is:
;;
;; 1. Find every definition that this project establishes
;; 2. Remove ones have more than one reference (the definition itself)
;; 3. Show the list

;; The first step is done with `elisp-unused--find-defined-things',
;; which heavily abuses `dumb-jump'.
;;
;; Looking for references of an identifier is done through
;; `elisp-unused--find-references'.

;;; Code:

(require 'xref)
(require 'dumb-jump)
(require 'projectile)

(require 'dash)
(require 's)

(defun elisp-unused--find-references (identifier)
  "Return references of IDENTIFIER.

This is actually suitable for use as an implementation of
`xref-backend-references'."
  (let ((variant (dumb-jump-pick-grep-variant))
        results)
    ;; For the signature of this function see eg.
    ;; `dumb-jump-parse-ag-response'.
    (setq results (funcall (plist-get variant :parse)
                           (shell-command-to-string
                            ;; And for the signature of this function see eg.
                            ;; `dumb-jump-generate-ag-command'.
                            (concat (funcall (plist-get variant :generate)
                                             identifier
                                             (buffer-file-name)
                                             (projectile-project-root)
                                             '("(\\(|\\s|')JJJ(\\s|\\))")
                                             "elisp"
                                             nil)
                                    ;; rg and ag ignore hidden files by default, which
                                    ;; isn't helpful here.
                                    (if (memq (plist-get variant :searcher)
                                              '(ag rg git-grep-plus-ag))
                                        " --hidden"
                                      "")))
                           (buffer-file-name)
                           ;; dumb-jump uses this to filter out the "current"
                           ;; reference, ie. the reference in the same file & on the
                           ;; same line as point. Since that's not applicable here, we
                           ;; can pass an impossible line number and make it return
                           ;; everything.
                           0))
    (cl-loop for ref in results
             collect (xref-make (plist-get ref :context)
                                (xref-make-file-location
                                 (plist-get ref :path)
                                 (plist-get ref :line) 0)))))

(defun elisp-unused--find-def (thing project)
  "Find definitions using THING in PROJECT.

If PROJECT is non-nil, look there instead."
  (let ((results (cl-letf (((symbol-function 'dumb-jump--get-symbol-start)
                            (lambda (&rest _) nil))
                           ((symbol-function 'dumb-jump-get-point-context)
                            (lambda (&rest _) nil))
                           ((symbol-function 'dumb-jump-get-point-line)
                            (lambda (&rest _) nil))
                           ((symbol-function 'dumb-jump-get-point-symbol)
                            (lambda (&rest _) thing))
                           ((symbol-function 'dumb-jump-get-ctx-type-by-language)
                            (lambda (&rest _) "function")))
                   ;; I don't know why this doesn't just turn up all
                   ;; definitions, but hey, it's called "dumb-jump"
                   ;; for a reason.
                   (dumb-jump-fetch-results
                    (or (buffer-file-name) default-directory)
                    (or project (projectile-project-root))
                    "elisp"
                    nil))))
    (plist-get results :results)))

(defun elisp-unused--find-defined-things (&optional project)
  "List all symbols that are defined in this project.

If PROJECT is non-nil, look there instead.

Results are returned in the form (IDENTIFIER . (:path PATH :line LINE))."
  (let (results)
    (setq
     results (append
              (elisp-unused--find-def "defun" project)
              (elisp-unused--find-def "defmacro" project))
     results (cl-loop
              for r in results
              when (s-starts-with? "(" (plist-get r :context))
              collect
              (list (--> (plist-get r :context)
                         ;; Make sure parens are balanced
                         (let ((n (- (cl-count ?\( it)
                                     (cl-count ?\) it))))
                           (cond ((> n 0)
                                  (concat it (make-string n ?\))))
                                 ;; More close parens than open parens means that
                                 ;; this form is probably not what we're looking for
                                 ((< n 0) nil)
                                 (t it))))
                    :path (plist-get r :path)
                    :line (plist-get r :line)))
     results (--filter (car it) results))
    (cl-loop for r in results
             when (car r)
             do (setf (car r)
                      (read (car r))))
    (setq results (--remove (let ((thing (car it)))
                              (or (not (listp thing))
                                  (not thing)
                                  (memq (car thing) '(cl-loop loop))
                                  (not (symbolp (cadr thing)))))
                            results))
    (cl-loop for r in results
             when (car r)
             do (setf (car r)
                      (format "%s" (nth 1 (car r)))))
    results))

(defun elisp-unused--find-unused-callables (&optional project)
  "Return list of unused callables in PROJECT.

Actually returns both the list of unused callables and the list
of all callables (as a cons cell (UNUSED-CALLABLES . LOCATION-ALIST)),
as the latter has the file location information in it."
  (let* ((location-alist (elisp-unused--find-defined-things project))
         (things (mapcar #'car location-alist))
         (len (length things))
         (default-directory (or project default-directory)))
    (cons (cl-remove
           nil
           (cl-loop with reporter = (make-progress-reporter
                                     "Looking for unused callables..." 0 len)
                    for thing being the elements of things using (index i)
                    collect
                    (progn
                      (progress-reporter-update reporter (1+ i))
                      (let ((count (length
                                    (elisp-unused--find-references thing))))
                        (unless (or (> count 1)
                                    (commandp (intern thing)))
                          (list thing count))))
                    finally do (progress-reporter-done reporter)))
          location-alist)))

(defun elisp-unused-list-unused-callables (&optional project)
  "List unused functions and macros in PROJECT."
  (interactive)
  (let* ((ret (elisp-unused--find-unused-callables project))
         (results (car ret))
         (location-alist (cdr ret))
         (default-directory (or project default-directory))
         (project default-directory))
    (redisplay)
    (with-current-buffer (get-buffer-create "*Elisp Unused*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (format
                 (propertize "%s unused callables defined in %s:\n\n"
                             'face
                             (list :inherit 'variable-pitch))
                 (length results)
                 (make-text-button
                  project nil
                  'follow-link t
                  'face '(link variable-pitch)
                  'action (lambda (&rest _)
                            (find-file project)))))
        (cl-loop for (func _) in results
                 do
                 ;; Establish a unique binding, otherwise all buttons
                 ;; will refer to the same variable.
                 (let* ((func func)
                        (button
                         (make-text-button
                          func nil
                          'follow-link t
                          'face 'link
                          'action (lambda (&rest _)
                                    ;; This will work even for files
                                    ;; that are not loaded.
                                    (let ((props (cdr (assoc func location-alist))))
                                      (find-file
                                       (expand-file-name
                                        (plist-get props :path)
                                        project))
                                      (goto-char (point-min))
                                      (forward-line (1- (plist-get props :line))))))))
                   (insert
                    (format "- %s\n" button))))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(provide 'elisp-unused)

;;; elisp-unused.el ends here
