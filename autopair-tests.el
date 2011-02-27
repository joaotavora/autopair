;;; autopair-tests.el --- Tests for autopair.el

;; Copyright (C) 2010  João Távora

;; Author: João Távora(defvar autopair-extra-tests nil) <joaotavora@gmail.com>
;; Keywords: emulations, convenience

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

;; Mini test framework for autopair.el

;;; Code:
(require 'autopair)
(setq autopair-extra-tests (list (list "       "
                                       "-----`-"
                                       #'autopair-extra-pair-p
                                       "-----y-"
                                       '((autopair-extra-pairs '(:everywhere ((?` . ?'))))))
                                 (list "\"     \""
                                       "-----`-"
                                       #'autopair-extra-pair-p
                                       "-----y-"
                                       '((autopair-extra-pairs '(:string ((?` . ?'))))))
                                 (list "   ` ' "
                                       "-----'-"
                                       #'autopair-extra-skip-p
                                       "-----y-"
                                       '((autopair-extra-pairs '(:everywhere ((?` . ?'))))))
                                 (list "  \"   \""
                                       "-`---`-"
                                       #'autopair-extra-pair-p
                                       "-----y-"
                                       '((autopair-extra-pairs '(:string ((?` . ?'))))))))



;; mini test-framework for the decision making predicates
;;
(defvar autopair-tests)
(setq autopair-tests (list (list " (())  "          ; contents
                                 "((((((("          ; input
                                 #'autopair-pair-p  ; predicate
                                 "yyyyyyy"          ; expected
                                 nil)               ; let-style-test-env
                           (list " ((()) "
                                 "((((((("
                                 #'autopair-pair-p
                                 "yyyyyyy")
                           (list " (())) "
                                 "((((((("
                                 #'autopair-pair-p
                                 "------y")
                           (list "()   ) "
                                 "---(---"
                                 #'autopair-pair-p
                                 "-------")
                           (list " ((()) "
                                 "----))-"
                                 #'autopair-skip-p
                                 "-------")
                           (list " (())  "
                                 "---))--"
                                 #'autopair-skip-p
                                 "---yy--")
                           (list " (())) "
                                 "---)))-"
                                 #'autopair-skip-p
                                 "---yyy-")
                           ;; some mixed paren situations
                           (list "  ()]  "
                                 "-(-----"
                                 #'autopair-pair-p
                                 "-y-----")
                           (list "  (])  "
                                 "-(-----"
                                 #'autopair-pair-p
                                 "-y-----")
                           (list "  ()]  "
                                 "-[-----"
                                 #'autopair-pair-p
                                 "-------")
                           (list "( ()]) "
                                 "-[-----"
                                 #'autopair-pair-p
                                 "-------")
                           (list "( ()]) "
                                 "-(-----"
                                 #'autopair-pair-p
                                 "-y-----")
                           (list " [([())  "
                                 "-----))--"
                                 #'autopair-skip-p
                                 "-----y---")
                           (list " [([])   "
                                 "-----)---"
                                 #'autopair-skip-p
                                 "-----y---")))

(defvar autopair-autowrap-tests)
(setq autopair-autowrap-tests (list
                               (list #'(lambda ()
                                         (insert "hello") (set-mark (point)) (beginning-of-buffer))
                                     "("
                                     #'(lambda ()
                                         (cons (buffer-substring-no-properties (point-min) (point-max))
                                               (point)))
                                     '("(hello)" . 2))
                               (list #'(lambda ()
                                         (insert "hello") (set-mark (point)) (beginning-of-buffer))
                                     ")"
                                     #'(lambda ()
                                         (cons (buffer-substring-no-properties (point-min) (point-max))
                                               (point)))
                                     '("(hello)" . 8))
                               (list #'(lambda ()
                                         (insert "hello") (set-mark (point)) (beginning-of-buffer)
                                         (exchange-point-and-mark))
                                     ")"
                                     #'(lambda ()
                                         (cons (buffer-substring-no-properties (point-min) (point-max))
                                               (point)))
                                     '("(hello)" . 8))
                               (list #'(lambda ()
                                         (insert "hello") (set-mark (point)) (beginning-of-buffer)
                                         (exchange-point-and-mark))
                                     "("
                                     #'(lambda ()
                                         (cons (buffer-substring-no-properties (point-min) (point-max))
                                               (point)))
                                     '("(hello)" . 2))))

(dolist (p '(autopair-pair-p autopair-skip-p autopair-extra-skip-p autopair-extra-pair-p))
  (put p 'autopair-test-charwise-predicate t))

(defun autopair-test (before
                      input
                      extractor-or-predicate)
    (if (stringp before) (insert before))
    (cond ((and (symbolp extractor-or-predicate)
                (get extractor-or-predicate 'autopair-test-charwise-predicate))
           (let* ((size (1- (point-max)))
                  (result (make-string size ?-)))
             (dotimes (i size)
               (goto-char (1+ i))
               (let ((autopair-inserted (aref input i)))
                 (when (and (not (eq autopair-inserted ?-))
                            (funcall extractor-or-predicate) (aset result i ?y)))))
             result))
          (t
           (funcall before)
           (if (functionp input)
               (funcall input)
             (let ((last-command-event (aref input 0)))
               (call-interactively (key-binding input) nil)
               (autopair-post-command-handler)))
           (funcall extractor-or-predicate))))

(defun autopair-run-tests (&optional suite)
  (interactive)
  (let ((passed 0)
        (failed 0))
    (with-output-to-temp-buffer "*autopair-tests*"
      (dolist (spec (or suite (append autopair-tests
                                      autopair-extra-tests
                                      autopair-autowrap-tests)))
        (let* ((fspec (fourth spec))
               (expected (or (and (functionp fspec) (funcall fspec))
                             fspec
                             t))
               (actual (condition-case e
                           (with-current-buffer (generate-new-buffer "*autopair-test*")
                             (emacs-lisp-mode)
                             (autopair-mode 1)
                             (transient-mark-mode 1)
                             (setq autopair-extra-pairs nil
                                   autopair-dont-pair nil
                                   autopair-handle-action-fns nil
                                   autopair-handle-wrap-action-fns nil)
                             (eval `(let ,(fifth spec)
                                      (autopair-test (first spec)
                                                     (second spec)
                                                     (third spec)))))
                         (error (error e)))))
          (condition-case err
              (progn (assert (equal actual expected)
                             'show-args
                             (format "test \"%s\" for input %s returned %s instead of %s\n"
                                     (first spec)
                                     (second spec)
                                     actual
                                     expected))
                     (incf passed))
            (error (progn
                     (princ (cadr err))
                     (incf failed))))))
      (princ (format "%s%s tests total, %s pass, %s failures"
                     (or (and (zerop failed) "") "\n\n")
                     (+ passed failed)
                     passed
                     failed)))
    (when noninteractive
      (with-current-buffer "*autopair-tests*"
        (princ (buffer-substring-no-properties (point-min) (point-max))))
      (kill-emacs failed))))


(provide 'autopair-tests)
;;; autopair-tests.el ends here

