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

;; Tests for autopair.el

;;; Code:
(require 'autopair)
(require 'ert)
(require 'ert-x)

;;;; Unit tests
;;;;
(ert-deftest fabricated-syntax-tables ()
  "Test that we can fabricate syntax tables for just one delimiter"
  (let ((opening-paren ?\()
        (closing-paren ?\))
        (opening-bracket ?\[)
        (closing-bracket ?\]))
    ;; First check that everything is as expected in the
    ;; `standard-syntax-table'
    ;;
    (with-syntax-table (standard-syntax-table)
      (should (equal (string-to-syntax "()")
                     (aref (syntax-table) opening-paren)))
      (should (equal (string-to-syntax ")(")
                     (aref (syntax-table) closing-paren)))
      (should (equal (string-to-syntax "(]")
                     (aref (syntax-table) opening-bracket)))
      (should (equal (string-to-syntax ")[")
                     (aref (syntax-table) closing-bracket))))
    ;; Now use `autopair-just-for-delim-syntax-table', testing that we
    ;; get rid of the "()" pair, but keep the "[]" pair and the quote delims
    ;;
    (with-syntax-table (autopair-just-for-delim-syntax-table opening-bracket)
      ;; Test that we got rid of the "()" pair...
      ;;
      (should (equal (string-to-syntax "w")
                          (aref (syntax-table) opening-paren)))
      (should (equal (string-to-syntax "w")
                     (aref (syntax-table) closing-paren)))
      (should (equal (string-to-syntax "(]")
                     (aref (syntax-table) opening-bracket)))
      (should (equal (string-to-syntax ")[")
                     (aref (syntax-table) closing-bracket)))
      (should (equal (string-to-syntax "\"")
                     (aref (syntax-table) ?\"))))
    (with-syntax-table (autopair-just-for-delim-syntax-table closing-bracket)
      (should (equal (string-to-syntax "w")
                     (aref (syntax-table) opening-paren)))
      (should (equal (string-to-syntax "w")
                     (aref (syntax-table) closing-paren)))
      (should (equal (string-to-syntax "(]")
                     (aref (syntax-table) opening-bracket)))
      (should (equal (string-to-syntax ")[")
                     (aref (syntax-table) closing-bracket)))
      (should (equal (string-to-syntax "\"")
                     (aref (syntax-table) ?\"))))))

(defun autopair-mock-syntax-ppss
  (toinsert table &optional wherepoint)
  (with-temp-buffer
    (with-syntax-table table
      (insert toinsert)
      (backward-char (or wherepoint (/ (buffer-size) 2)))
      (syntax-ppss))))

(ert-deftest test-syntax-ppss-with-fabricated-syntax ()
  (should (eq 4
              (car (autopair-mock-syntax-ppss "((([])))" (standard-syntax-table)))))
  (should (eq 1
              (car (autopair-mock-syntax-ppss "((([])))" (autopair-just-for-delim-syntax-table ?\[)))))
  (should (eq 3
              (car (autopair-mock-syntax-ppss "((([])))" (autopair-just-for-delim-syntax-table ?\()))))
  (should (equal (autopair-mock-syntax-ppss "\"((()))\"" (standard-syntax-table))
                 (autopair-mock-syntax-ppss "\"((()))\"" (autopair-just-for-delim-syntax-table ?\()))))



;;;; Functional tests
;;;;
(defmacro define-autopair-simple-predicate-test (name-or-name-and-ert-args fixture input predicate expectation &optional bindings)
  (declare (indent defun))
  (let ((name name-or-name-and-ert-args)
        (ert-args '()))
    (when (listp name)
      (setq ert-args (rest name))
      (setq name (first name)))
    `(ert-deftest ,(intern (concat "autopair-simple-predicate-test-" (symbol-name name))) ()
       ,(format "%sWith \"%s\", call `%s' for \"%s\". Should get \"%s\""
                "" ;; TODO implmement docstrings
                fixture
                (symbol-name predicate)
                input
                expectation)
       ,@ert-args
       (with-temp-buffer
         (let ,bindings
           (autopair-mode 1)
           (insert ,fixture)
           (let* ((size (1- (point-max)))
                  (result (make-string size ?-)))
             (dotimes (i size)
               (goto-char (1+ i))
               (let ((autopair-inserted (aref ,input i)))
                 (when (and (not (eq autopair-inserted ?-))
                            (funcall #',predicate)
                            (aset result i ?y)))))
             (should (string= result ,expectation))))))))

(defmacro define-autopair-functional-test (name-or-name-and-ert-args fixture-fn input expected-text expected-point &optional bindings)
  (declare (indent defun))
  (let ((name name-or-name-and-ert-args)
         (ert-args '()))
    (when (listp name)
      (setq ert-args (rest name))
      (setq name (first name)))
    `(ert-deftest ,(intern (concat "autopair-functional-test-" (symbol-name name))) ()
       ,(format "%s: see test definition" name)
       ,@ert-args
       (with-temp-buffer
         (let ,bindings
           (autopair-mode 1)
           (funcall ,fixture-fn)
           (cond ((and (symbolp ,input)
                       (commandp ,input))
                  (ert-simulate-command ,input))
                 ((stringp ,input)
                  (let ((last-command-event (aref ,input 0)))
                    (call-interactively (key-binding ,input) nil)
                    (autopair-post-command-handler))))
           (should (string= (buffer-substring-no-properties (point-min) (point-max)) ,expected-text))
           (should (eql (point) ,expected-point)))))))

;;; basic tests
;;;
(define-autopair-simple-predicate-test balanced-situation
  " (())  " "(((((((" autopair-pair-p "yyyyyyy")

(define-autopair-simple-predicate-test too-many-openings
  " ((()) " "(((((((" autopair-pair-p "yyyyyyy")

(define-autopair-simple-predicate-test too-many-closings
  " (())) " "(((((((" autopair-pair-p "------y")

(define-autopair-simple-predicate-test too-many-closings-2
  "()   ) " "---(---" autopair-pair-p "-------")

(define-autopair-simple-predicate-test balanced-autoskipping
  " (())  " "---))--" autopair-skip-p "---yy--")

(define-autopair-simple-predicate-test too-many-openings-autoskipping
  " ((()) " "----))-" autopair-skip-p "-------")

(define-autopair-simple-predicate-test too-many-closings-autoskipping
  " (())) " "---)))-" autopair-skip-p "---yyy-")

(define-autopair-simple-predicate-test mixed-paren-1
  "  ()]  " "-(-----" autopair-pair-p "-y-----")

(define-autopair-simple-predicate-test mixed-paren-2
  "  (])  " "-(-----" autopair-pair-p "-y-----")

(define-autopair-simple-predicate-test find-matching-different-paren-type
  "  ()]  " "-[-----" autopair-pair-p "-------")

(define-autopair-simple-predicate-test find-matching-different-paren-type-inside-list
  "( ()]) " "-[-----" autopair-pair-p "-------")

(define-autopair-simple-predicate-test ignore-different-unmatching-paren-type
  "( ()]) " "-(-----" autopair-pair-p "-y-----")

(define-autopair-simple-predicate-test autopair-keep-least-amount-of-mixed-unbalance
  "( ()]  " "-(-----" autopair-pair-p "-y-----")

(define-autopair-simple-predicate-test dont-autopair-to-resolve-mixed-unbalance
  "( ()]  " "-[-----" autopair-pair-p "-------")

(define-autopair-simple-predicate-test (autopair-so-as-not-to-worsed-unbalance-situation
                                        :expected-result :failed)

  "( (])  " "-[-----" autopair-pair-p "-y-----")

(define-autopair-simple-predicate-test skip-over-partially-balanced
  " [([])   " "-----)---" autopair-skip-p "-----y---")

(define-autopair-simple-predicate-test only-skip-over-at-least-partially-balanced-stuff
  " [([())  " "-----))--" autopair-skip-p "-----y---")

;;; extra pairs tests
;;;
(define-autopair-simple-predicate-test pair-of-backtick-and-quote
  "       " "-----`-" autopair-extra-pair-p "-----y-"
  ((autopair-extra-pairs '(:everywhere ((?` . ?'))))))

(define-autopair-simple-predicate-test pair-backtick-and-quote-but-only-inside-string
  "\"     \"  " "-----`--`" autopair-extra-pair-p "-----y---"
  ((autopair-extra-pairs '(:string     ((?` . ?'))))))

(define-autopair-simple-predicate-test skip-backtick-and-quote
  "   ` ' " "-----'-" autopair-extra-skip-p "-----y-"
  ((autopair-extra-pairs '(:everywhere ((?` . ?'))))))

(define-autopair-simple-predicate-test skip-backtick-and-quote-but-only-inside-string
  "  \"   \"" "-`---`-" autopair-extra-pair-p "-----y-"
  ((autopair-extra-pairs '(:string     ((?` . ?'))))))

;;; autowrap tests
;;;
(define-autopair-functional-test autowrap-from-beginning
  #'(lambda ()
      (insert "hello") (set-mark (point)) (beginning-of-buffer))
  "(" "(hello)"  2)
(define-autopair-functional-test autowrap-to-end
  #'(lambda ()
          (insert "hello") (set-mark (point)) (beginning-of-buffer))
  ")" "(hello)"  8)
(define-autopair-functional-test autowrap-from-end-stay-at-end
  #'(lambda ()
          (insert "hello") (set-mark (point)) (beginning-of-buffer)
          (exchange-point-and-mark))
  ")" "(hello)"  8)
(define-autopair-functional-test autowrap-from-end-go-to-beginning
  #'(lambda ()
          (insert "hello") (set-mark (point)) (beginning-of-buffer)
          (exchange-point-and-mark))
  "(" "(hello)"  2)


(define-autopair-functional-test autowrap-by-closing-inside-mixed-parens
  ;;; googlecode issue 49
  #'(lambda ()
      (insert "[hello]")
      (set-mark 2)
      (backward-char))
  "}"
  "[{hello}]"
  9)

(define-autopair-functional-test autowrap-by-opening-inside-mixed-parens
  #'(lambda ()
      (insert "[hello]")
      (goto-char 2)
      (set-mark 7))
  "{"
  "[{hello}]"
  3)


(provide 'autopair-tests)
;;; autopair-tests.el ends here
