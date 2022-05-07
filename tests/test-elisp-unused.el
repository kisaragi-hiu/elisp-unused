;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'elisp-unused)

(defvar temp-dir)

(defun init (&optional dir)
  "Set up testing files in DIR and set `temp-dir' to it.

If DIR is nil, create it."
  (setq temp-dir (or dir (make-temp-file "test" t))
        default-directory temp-dir)
  ;; Mark this folder as project root
  (with-temp-file "TAGS"
    (insert ""))
  (with-temp-file "main.el"
    (insert "(defun used/return-one () 1)
(defvar unused/two (+ (used/return-one) 1))
(defun unused/a () 10)
(defun unused/another () 10)
")))

(defun teardown ()
  "Remove testing files"
  (delete-directory temp-dir t))

(defmacro test (&rest body)
  "Set up the testing file(s) and run BODY in there."
  `(let* ((temp-dir (make-temp-file "test" t))
          (default-directory temp-dir))
     (unwind-protect
         (progn
           (init)
           ,@body)
       (teardown))))

(describe ""
  (before-all
    (init))
  (after-all
    (teardown))
  (it "Finds unused callables"
    (expect (car (elisp-unused--find-unused-callables))
            :to-have-same-items-as
            '(("unused/a" 1)
              ("unused/another" 1))))
  (it "Finds defined callables"
    (expect (elisp-unused--find-defined-things)
            :to-have-same-items-as
            `(("used/return-one" :path ,(expand-file-name "main.el") :line 1)
              ("unused/a" :path ,(expand-file-name "main.el") :line 3)
              ("unused/another" :path ,(expand-file-name "main.el") :line 4)))))

;; Local Variables:
;; mode: lisp-interaction
;; End:
