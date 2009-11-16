;;; autopair.el --- Automagically pair braces and quotes like TextMate

;; Copyright (C) 2009 Joao Tavora

;; Author: Joao Tavora <joaotavora [at] gmail.com>
;; Keywords: convenience, emulations
;; X-URL: http://autopair.googlecode.com
;; URL: http://autopair.googlecode.com
;; EmacsWiki: AutoPairs
;; Version: 0.2
;; Revision: $Rev$ ($LastChangedDate$)

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
;;
;; Another stab at making braces and quotes pair like in
;; TextMate:
;; 
;; * Opening braces/quotes are autopaired;
;; * Closing braces/quotes are autoskipped;
;; * Backspacing an opening brace/quote autodeletes its adjacent pair.
;;
;; Autopair deduces from the current syntax table which characters to
;; pair, skip or delete.
;;
;;; Installation:
;;
;;   (require 'autopair)
;;   (autopair-global-mode) ;; to enable in all buffers
;;
;; To enable autopair in just some types of buffers, comment out the
;; `autopair-global-mode' and put autopair-mode in some major-mode
;; hook, like:
;;
;; (add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))
;;
;; Alternatively, do use `autopair-global-mode' and create
;; *exceptions* using the `autopair-dont-activate' local variable,
;; like:
;;
;; (add-hook 'c-mode-common-hook #'(lambda () (setq autopair-dont-activate t)))
;;
;;; Use:
;; 
;; The extension works by rebinding the braces and quotes keys, but
;; can still be minimally intrusive, since the original binding is
;; always called as if autopair did not exist.
;;
;; The decision of which keys to actually rebind is taken at
;; minor-mode activation time, based on the current major mode's
;; syntax tables. To achieve this kind of behaviour, an emacs
;; variable `emulation-mode-map-alists' was used.
;;
;; If you set `autopair-pair-criteria' and `autopair-skip-criteria' to
;; 'help-balance (which, by the way, is the default), braces are not
;; autopaired/autoskiped in all situations; the decision to autopair
;; or autoskip a brace is taken according to the following table:
;;
;;  +---------+------------+-----------+-------------------+
;;  | 1234567 | autopair?  | autoskip? | notes             |
;;  +---------+------------+-----------+-------------------+
;;  |  (())   |  yyyyyyy   |  ---yy--  | balanced          |
;;  +---------+------------+-----------+-------------------+
;;  |  (()))  |  ------y   |  ---yyy-  | too many closings |
;;  +---------+------------+-----------+-------------------+
;;  |  ((())  |  yyyyyyy   |  -------  | too many openings |
;;  +---------+------------+-----------+-------------------+
;;
;; The table is read like this: in a buffer with 7 characters laid out
;; like the first column, an "y" marks points where an opening brace
;; is autopaired and in which places would a closing brace be
;; autoskiped. Quote pairing tries to support similar "intelligence".
;;
;; For further customization have a look at `autopair-dont-pair',
;; `autopair-handle-action-fns' and `autopair-extra-pair'.
;;
;; `autopair-dont-pair' lets you define special cases of characters
;; you don't want paired.  Its default value skips pairing
;; single-quote characters when inside a comment literal, even if the
;; language syntax tables does pair these characters.
;;
;; As a further example, to also prevent the '{' (opening brace)
;; character from being autopaired in C++ comments use this in your
;; .emacs.
;;
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;                (push ?{
;;                      (getf autopair-dont-pair :comment))))
;;
;; `autopair-handle-action-fns' lets you override/extend the actions
;; taken by autopair after it decides something must be paired,skipped
;; or deleted. To work with triple quoting in python mode, you can use
;; this for example:
;;
;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;               (setq autopair-handle-action-fns
;;                     (list #'autopair-default-handle-action
;;                           #'autopair-python-triple-quote-action))))
;;
;; `autopair-extra-pair' lets you define extra pairing and skipping
;; behaviour for pairs not programmed into the syntax table. (Work-in-progress)
;;                      
;;; Bugs:
;;
;; * Quote pairing/skipping inside comments is not perfect...
;;
;;; Credit:
;;
;; Thanks Ed Singleton for early testing.
;;
;;; Code:

;; requires
(require 'cl)

;; variables
(defvar autopair-pair-criteria 'help-balance
  "If non-nil, be more criterious when pairing opening brackets.")

(defvar autopair-skip-criteria 'help-balance
  "If non-nil, be more criterious when skipping closing brackets.")

(defvar autopair-emulation-alist nil
  "A dinamic keymap for autopair set mostly from the current
  syntax table.")
(make-variable-buffer-local 'autopair-emulation-alist)

(defvar autopair-dont-activate nil
  "If non-nil `autopair-global-mode' does not activate in buffer")
(make-variable-buffer-local 'autopair-dont-activate)

(defvar autopair-extra-pairs `(:comment ((?` . ?')))
  "Extra pairs for which to use pairing.

It's a Common-lisp-style even-numbered property list, each pair
of elements being of the form (TYPE , PAIRS). PAIRS is a mixed
list whose elements can be cons cells or character literals. Cons
cells look like (OPENING . CLOSING) and are paired like
parenthesis. Single characters are paired like quotes.

TYPE can be one of:

:string : whereby PAIRS will be considered only when inside a
          string literal

:comment : whereby PAIRS will be considered only when inside a comment

:code : whereby PAIRS will be considered only when outisde a
        string and a comment.

:everywhere : whereby PAIRS will be considered in all situations")
(make-variable-buffer-local 'autopair-dont-pair)

(defvar autopair-dont-pair `(:string (?') :comment  (?'))
  "Characters for which to skip any pairing behaviour.

This variable overrides `autopair-pair-criteria' and
`autopair-extra-pairs'. It does not
  (currently) affect the skipping behaviour.

It's a Common-lisp-style even-numbered property list, each pair
of elements being of the form (TYPE , CHARS). CHARS is a list of
characters and TYPE can be one of:

:string : whereby characters in CHARS will not be autopaired when
          inside a string literal

:comment : whereby characters in CHARS will not be autopaired when
          inside a comment

:never : whereby characters in CHARS won't even have their
         bindings replaced by autopair's. This particular option
         should be used for troubleshooting and requires
         `autopair-mode' to be restarted to have any effect.")
(make-variable-buffer-local 'autopair-dont-pair)

(defvar autopair-action nil
  "Autopair action decided on by last interactive autopair command, or nil.

ACTION is one of `opening',`insert-quote' or `skip-quote' or
`backspace'. PAIR is an element of `autopair-pairs'. POS-BEFORE
is value of point before action command took place .")
(make-variable-buffer-local 'autopair-action)


(defvar autopair-handle-action-fns '()
  "List of autopair handlers to run *instead* of the default handler.

Each element is a function taking three arguments (ACTION, PAIR
and POS-BEFORE), which are the three elements of the
`autopair-action' variable, which see.

If non-nil, these functions are called *instead* of the single
fucntion `autopair-default-handle-action', so use this variable
to specify special behaviour. To also run the default behaviour,
be sure to include the default function in the list.")


;; minor mode and global mode
;; 
(define-globalized-minor-mode autopair-global-mode autopair-mode autopair-on)

(defun autopair-on () (unless autopair-dont-activate (autopair-mode 1)))

(define-minor-mode autopair-mode
  "Automagically pair braces and quotes like in TextMate."
  nil " pair" nil
  (cond (autopair-mode
         ;; Setup the dynamic emulation keymap
         ;;
         (let ((map (make-sparse-keymap)))
           (define-key map [remap delete-backward-char] 'autopair-backspace)
           (define-key map [remap backward-delete-char-untabify] 'autopair-backspace)
           (define-key map (kbd "<backspace>") 'autopair-backspace)
           (define-key map [backspace] 'autopair-backspace)
           (dotimes (char 256) ;; only searches the first 256 chars, TODO: is this enough/toomuch/stupid?
             (unless (member char
                             (getf autopair-dont-pair :never))
               (let* ((syntax-entry (aref (syntax-table) char))
                      (class (and syntax-entry
                                  (syntax-class syntax-entry)))
                      (pair (and syntax-entry
                                 (cdr syntax-entry))))
                 (cond ((eq class (car (string-to-syntax "(")))
                        (define-key map (string char) 'autopair-insert-opening)
                        (define-key map (string pair) 'autopair-skip-close-maybe))
                       ((eq class (car (string-to-syntax "\"")))
                        (define-key map (string char) 'autopair-insert-or-skip-quote))))))
           (setq autopair-emulation-alist (list (cons t map))))
         
         (setq autopair-action nil)
         (add-hook 'emulation-mode-map-alists 'autopair-emulation-alist nil)
         (add-hook 'post-command-hook 'autopair-post-command-handler 'append 'local))
        (t
         (setq autopair-emulation-alist nil)
         (remove-hook 'emulation-mode-map-alists 'autopair-emulation-alist)
         (remove-hook 'post-command-hook         'autopair-post-command-handler 'local))))

;; useful functions, mostly decision logic
;;

(defun autopair-syntax-ppss ()
  (let ((quick-syntax-info (syntax-ppss)))

    (cond (;; inside a string, recalculate
           (nth 3 quick-syntax-info)
           (cons (parse-partial-sexp (1+ (nth 8 quick-syntax-info)) (point))
                 :string))
          ((nth 4 quick-syntax-info)
           (cons (parse-partial-sexp (1+ (nth 8 quick-syntax-info)) (point))
                 :comment))
          (t
           (cons quick-syntax-info
                 :code)))))

(defun autopair-fallback (&optional fallback-keys)
  (let ((autopair-emulation-alist nil)
        (command (or (key-binding (this-single-command-keys))
                     (key-binding fallback-keys))))
    (call-interactively command)))

(defun autopair-document-bindings (&optional fallback-keys)
  (concat 
   "Works by scheduling possible autopair behaviour, then calls
original command as if autopair didn't exist"
   (when (eq this-command 'describe-key)
     (let* ((autopair-emulation-alist nil)
            (command (or (key-binding (this-single-command-keys))
                         (key-binding fallback-keys))))
       (when command
         (format ", which in this case is `%s'" command))))
   "."))

(defun autopair-skip-p ()
  (interactive)
  (let* ((syntax-info-and-where-sym (autopair-syntax-ppss))
         (syntax-info (car syntax-info-and-where-sym)))
    (and (eq (char-after (point)) last-input-event)
         (cond ((eq autopair-skip-criteria 'help-balance)
                (save-excursion
                  (autopair-up-list syntax-info last-input-event)))
               ((eq autopair-skip-criteria 'need-opening)
                (save-excursion
                  (condition-case err
                      (progn
                        (backward-list)
                        t)
                    (error nil))))
               (t
                t)))))

(defun autopair-extra-skip-p ()
  (let* ((syntax-info-and-where-sym (autopair-syntax-ppss))
         (syntax-info (car syntax-info-and-where-sym))
         (where-sym (cdr syntax-info-and-where-sym)))
    (and (some #'(lambda (sym)
                   (autopair-exception-p where-sym sym autopair-extra-pairs #'cdr))
               '(:comment :string :code :everywhere))
         (save-excursion
           (condition-case err
               (backward-sexp (point-max))
             (error
              (goto-char (third err))))
           (search-forward (make-string 1 (autopair-find-pair last-input-event 'by-closing-delim))
                           (point)
                           'noerror)))))
  
(defun autopair-following-quote-p (syntax-info)
  (nth 5 syntax-info))

(defun autopair-exception-p (where-sym exception-where-sym blacklist &optional fn)
  (and (or (eq exception-where-sym :everywhere)
           (eq exception-where-sym where-sym)) 
       (member last-input-event
               (if fn
                   (mapcar fn (getf blacklist exception-where-sym))
                 (getf blacklist exception-where-sym)))))

(defun autopair-up-list (syntax-info &optional input-event)
  "Try to uplist as most as reasonably possible.

Return nil if something prevented up-listing. If inside nested
lists of mixed parethesis types, finding a matching parenthesis
of a mixed-type is considered OK, and uplisting stops there."
  (condition-case nil
      (let ((howmany (car syntax-info)))
        (while (and (/= howmany 0)
                    (or (null input-event)
                        (condition-case err
                            (progn
                              (scan-sexps (point) (point-max))
                              (error err))
                          (error (eq input-event (char-before (third err)))))))
          (goto-char (scan-lists (point) 1 1))
          (decf howmany))
        (point))
    (error nil)))

(defun autopair-pair-p ()
  (let* ((syntax-info-and-where-sym (autopair-syntax-ppss))
         (syntax-info (car syntax-info-and-where-sym))
         (where-sym (cdr syntax-info-and-where-sym)))
    (and (not (some #'(lambda (sym)
                        (autopair-exception-p where-sym sym autopair-dont-pair))
                    '(:string :comment :code :everywhere)))
         (cond ((eq autopair-pair-criteria 'help-balance)
                (and (not (autopair-following-quote-p syntax-info))
                     (save-excursion
                       (autopair-up-list syntax-info)
                       (condition-case err
                           (progn
                             (forward-sexp (point-max))
                             t)
                         (error
                          ;; if `forward-sexp' returned an error,
                          ;; typically we don't want to autopair,
                          ;; unless...
                          ;;
                          ;; 1. `forward-sexp' stopped at a parenthesis
                          ;;    of a different type than
                          ;;    `last-input-event'
                          ;;    
                          ;; 2. The error is of type "containing
                          ;;    expression ends prematurely", which means
                          ;;    we're in the "too-many-openings"
                          ;;    situation and thus want to autopair.
                          (or (not (eq (autopair-find-pair last-input-event)
                                       (char-after (third err))))
                              (not (string-match "prematurely" (second err))))
                          ;; (eq (fourth err) (point-max))
                          )))))
               ((eq autopair-pair-criteria 'always)
                t)
               (t
                (not (autopair-following-quote-p))
                )))))

(defun autopair-extra-pair-p ()
  (let* ((syntax-info-and-where-sym (autopair-syntax-ppss))
         (syntax-info (car syntax-info-and-where-sym))
         (where-sym (cdr syntax-info-and-where-sym)))
    (some #'(lambda (sym)
                (autopair-exception-p where-sym sym autopair-extra-pairs #'car))
          '(:everywhere :comment :string :code))))

(defun autopair-find-pair (&optional delim by-closing-delim-p)
  (let ((syntax-entry (aref (syntax-table) (or delim
                                               last-input-event))))
    (cond ((eq (syntax-class syntax-entry) (car (string-to-syntax "(")))
           (cdr syntax-entry))
          ((eq (syntax-class syntax-entry) (car (string-to-syntax "\"")))
           delim)
          (t
           nil))))


;; interactive
;; 
(defun autopair-insert-or-skip-quote ()
  (interactive)
  (let* ((syntax-info-and-where-sym (autopair-syntax-ppss))
         (syntax-info (car syntax-info-and-where-sym))
         (where-sym (cdr syntax-info-and-where-sym))
         ;; inside-string may the quote character itself or t if this is a "generically terminated string"
         (inside-string (eq where-sym :string)))
    (unless (autopair-following-quote-p syntax-info)
      (cond (;; decides whether to skip the quote...
             ;; 
             (and (eq last-input-event (char-after (point)))
                  (or
                   ;; ... if we're already inside a string and the
                   ;; string starts with the character just inserted,
                   ;; or it's a generically terminated string
                   (and inside-string
                        (or (eq inside-string t)
                            (eq last-input-event inside-string)))
                   ;; ... if we're in a comment in a quote-after-quote
                   ;; situation (the inside-string criteria does not
                   ;; work here...)
                   (and (nth 4 syntax-info) 
                        (eq last-input-event (char-after (1- (point)))))))
             (setq autopair-action (list 'skip-quote last-input-event (point))))
            (;; decides whether to pair, i.e do *not* pair the quote if...
             ;; 
             (not
              (or
               ;; ... inside a generic string
               (eq inside-string t)
               ;; inside a string terminated by this char
               (eq last-input-event inside-string)
               ;; comment-disabled is true here or string-disable is
               ;; true here this last one is only useful if we're in a
               ;; string terminated by a character other than
               ;; `last-input-event'.
               (some #'(lambda (sym)
                         (autopair-exception-p where-sym sym autopair-dont-pair))
                     '(:comment :string))))
             (setq autopair-action (list 'insert-quote last-input-event (point))))))
    (autopair-fallback)))

  (put 'autopair-insert-or-skip-quote 'function-documentation
     '(concat "Insert or possibly skip over a quoting character.\n\n"
              (autopair-document-bindings)))

(defun autopair-insert-opening ()
  (interactive)
  (when (autopair-pair-p)
    (setq autopair-action (list 'opening (autopair-find-pair) (point))))
  (autopair-fallback))
(put 'autopair-insert-opening 'function-documentation
     '(concat "Insert opening delimiter and possibly automatically close it.\n\n"
              (autopair-document-bindings)))

(defun autopair-skip-close-maybe ()
  (interactive)
  (when (autopair-skip-p)
    (setq autopair-action (list 'closing last-input-event (point))))
  (autopair-fallback))
(put 'autopair-skip-close-maybe 'function-documentation
     '(concat "Insert or possibly skip over a closing delimiter.\n\n"
               (autopair-document-bindings)))

(defun autopair-backspace ()
  (interactive)
    (when (char-before)
      (setq autopair-action (list 'backspace (autopair-find-pair (char-before)) (point))))
  (autopair-fallback (kbd "DEL")))
(put 'autopair-backspace 'function-documentation
     '(concat "Possibly delete a pair of paired delimiters.\n\n"
              (autopair-document-bindings (kbd "DEL"))))

;; post-command-hook stuff
;;
(defun autopair-post-command-handler ()
  "Inserts,deletes or skips over pairs based on `autopair-action'. "
  (let ((action (first autopair-action))
        (pair (second autopair-action))
        (pos-before (third autopair-action)))
    (when (and action
               pair
               pos-before)
      (if autopair-handle-action-fns
          (mapc #'(lambda (fn)
                    (funcall fn action pair pos-before))
                autopair-handle-action-fns)
        (autopair-default-handle-action action pair pos-before))))
    (setq autopair-action nil))

(defun autopair-default-handle-action (action pair pos-before)
  (cond (;; automatically insert closing delimiter
         (eq 'opening action)
         (insert pair)
         (backward-char 1))
        (;; automatically insert closing quote delimiter 
         (eq 'insert-quote action)
         (insert pair)
         (backward-char 1))
        (;; automatically skip oper closer quote delimiter
         (eq 'skip-quote action)
         (delete-char 1))
        (;; skip over newly-inserted-but-existing closing delimiter
         ;; (normal case)
         (eq 'closing action)
         (delete-char 1))
        (;; autodelete closing delimiter
         (and (eq 'backspace action)
              (eq pair (char-after (point))))
         (delete-char 1))))


;; example python triple quote helper for

(defun autopair-python-triple-quote-action (action pair pos-before)
  (cond ((and (eq 'insert-quote action)
              (string= (buffer-substring (- (point) 3)
                                         (point))
                       (make-string 3 pair)))
         (save-excursion (insert (make-string 2 pair))))
        ((and (eq 'backspace action)
              (string= (buffer-substring (- (point) 2)
                                         (+ (point) 2))
                       (make-string 4 pair)))
         (delete-region (- (point) 2)
                        (+ (point) 2)))
        ((and (eq 'skip-quote action)
              (string= (buffer-substring (point)
                                         (+ (point) 2))
                       (make-string 2 pair)))
         (forward-char 2))
        (t
         t)))

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
                           (list " (())  "
                                 "---))--"
                                 #'autopair-skip-p
                                 "---yy--")
                           (list " (())) "
                                 "---)))-"
                                 #'autopair-skip-p
                                 "---yyy-")
                           ;; a mixed paren situations
                           (list "  ()]  "
                                 "-(-----"
                                 #'autopair-pair-p
                                 "-y-----")
                           (list " [([())  "
                                 "-----))--"
                                 #'autopair-skip-p
                                 "-----y---")))

(setq autopair-extra-tests (list
                            ;; test the extra pairs
                            ;; (list "       "
                            ;;       "-----`-"
                            ;;       #'autopair-extra-pair-p
                            ;;       "-----y-"
                            ;;       '((autopair-extra-pairs '(:everywhere ((?` . ?'))))))
                            ;; (list "\"     \""
                            ;;       "-----`-"
                            ;;       #'autopair-extra-pair-p
                            ;;       "-----y-"
                            ;;       '((autopair-extra-pairs '(:string ((?` . ?'))))))
                            (list "   ` ' "
                                  "-----'-"
                                  #'autopair-extra-skip-p
                                  "-----y-"
                                  '((autopair-extra-pairs '(:everywhere ((?` . ?'))))))
                            ;; (list "\"     \""
                            ;;       "-----`-"
                            ;;       #'autopair-extra-skip-p
                            ;;       "-----y-"
                            ;;       '((autopair-extra-pairs '(:string ((?` . ?'))))))
                            ))

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
      (dolist (spec (or suite autopair-tests))
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


(provide 'autopair)
;;; autopair.el ends here
