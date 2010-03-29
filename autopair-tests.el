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



(provide 'autopair-tests)
;;; autopair-tests.el ends here

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

(defun autopair-test (buffer-contents
                      input
                      predicate)
  (with-temp-buffer
    (autopair-mode t)
    (insert buffer-contents)
    (let* ((size (1- (point-max)))
           (result (make-string size ?-)))
      (dotimes (i size)
        (goto-char (1+ i))
        (let ((last-input-event (aref input i)))
          (when (and (not (eq last-input-event ?-))
                     (funcall predicate) (aset result i ?y)))))
      result)))

(defun autopair-run-tests (&optional suite)
  (interactive)
  (let ((passed 0)
        (failed 0))
    (with-output-to-temp-buffer "*autopair-tests*"
      (dolist (spec (or suite (append autopair-tests
                                      autopair-extra-tests)))
        (condition-case err
            (progn (assert (equal
                            (condition-case nil\
                                (eval `(let ,(fifth spec)
                                         (autopair-test (first spec)
                                                        (second spec)
                                                        (third spec))))
                              (error "error"))
                            (fourth spec))
                           'show-args
                           (format "test \"%s\" for input %s returned %%s instead of %s\n"
                                   (first spec)
                                   (second spec)
                                   (fourth spec)))
                   (incf passed))
          (error (progn
                   (princ (cadr err))
                   (incf failed))))
        )
      (princ (format "\n\n%s tests total, %s pass, %s failures"
                     (+ passed failed)
                     passed
                     failed)))))
