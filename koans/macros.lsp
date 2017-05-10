;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


; A Lisp macro is like a function which takes an input Lisp form
; and produces a new output Lisp form.  Calling the macro
; first produces the new form, and then evaluates it in the context
; of the macro call.  The first phase, the creation of the new
; macro form, is called 'macro expansion'.

(defmacro repeat-2 (f) (list 'progn f f))

(define-test test-macro-expands
    "ASSERT-EXPANDS checks the expanded macro form against expectation."
  (assert-expands
   '(progn (do-something arg1 arg2) (do-something arg1 arg2))
   (repeat-2 (do-something arg1 arg2)))

  (assert-expands
   ____
   (repeat-2 (setf x (+ 1 x)))))


;; ----


(define-test test-backtick-form
    "Backtick (`) form is much like single-quote (') form, except that subforms
     preceded by a comma (,) are evaluated, rather than left as literals."
  (let ((num 5)
        (word 'dolphin))
    (true-or-false? ___  (equal '(1 3 5) `(1 3 5)))
    (true-or-false? ___  (equal '(1 3 5) `(1 3 num)))
    (assert-equal ____ `(1 3 ,num))
    (assert-equal ____ `(word ,word ,word word))))


(define-test test-at-form
    "The at form, (@) in the backtick context splices list variables into
     the form."
  (let ((axis '(x y z)))
    (assert-equal '(x y z) axis)
    (assert-equal '(the axis are (x y z)) `(the axis are ,axis))
    (assert-equal '(the axis are x y z) `(the axis are ,@axis)))
  (let ((coordinates '((43.15 77.6) (42.36 71.06))))
    (assert-equal ____
      `(the coordinates are ,coordinates))
    (assert-equal ____
      `(the coordinates are ,@coordinates))))


;; On Gensym: based on ideas from common lisp cookbook.

  ; Sets SYM1 and SYM2 to VAL.
(defmacro double-setf-BAD (sym1 sym2 val)
  `(progn (setf ,sym1 ,val) (setf ,sym2 ,val)))

(define-test test-no-gensym
    "Macro expansions may introduce difficult-to-see interactions."
  (let ((x 0)
        (y 0))
    (double-setf-BAD x y 10)
    (assert-equal x 10)
    (assert-equal y 10))

  (let ((x 0)
        (y 0))
    (double-setf-BAD x y (+ x 100))
    (assert-equal x ____)
    (assert-equal y ____)))

  ; Sets SYM1 and SYM2 to VAL.
(defmacro double-setf-SAFER (sym1 sym2 val)
  (let ((new-fresh-symbol (gensym)))
    `(let ((,new-fresh-symbol ,val))
       (progn (setf ,sym1 ,new-fresh-symbol) (setf ,sym2 ,new-fresh-symbol)))))

(define-test test-with-gensym
    "GENSYM creates a new symbol."
  (let ((x 0)
        (y 0))
    (double-setf-SAFER x y 10)
    (assert-equal x 10)
    (assert-equal y 10))

  (let ((x 0)
        (y 0))
    (double-setf-SAFER x y (+ x 100))
    (assert-equal x ____)
    (assert-equal y ____)))


;; ----

(defvar *log* nil)

(defmacro log-form (form)
    "Records the body form to the list *LOG* and then evalues the body normally."
  `(let ((retval ,form))
     (push ',form *log*)
     retval))

(define-test test-basic-log-form
    "Illustrates how the basic LOG-FORM macro above works."
  (assert-equal 1978 (* 2 23 43))
  (assert-equal nil *log*)
    "LOG-FORM does not interfere with the usual return value."
  (assert-equal 1978 (log-form (* 2 23 43)))
    "LOG-FORM records the code which it has been passed."
  (assert-equal ___ (length *log*))
  (assert-equal ___ (first *log*))
    "Macros evaluating to more macros is OK, if confusing."
  (assert-equal 35 (log-form (log-form (- 2013 1978))))
  (assert-equal 3 (length *log*))
  (assert-equal '(log-form (- 2013 1978)) (first *log*))
  (assert-equal '(- 2013 1978) (second *log*)))

; Now you must write a more advanced LOG-FORM, that also records the value
; returned by the form.

(defvar *log-with-value* nil)

  ;; You must write this macro.
(defmacro log-form-with-value (form)
    "Records the body FORM and its evaluated return-value to the list
     *LOG-WITH-VALUE*, and returns the return-value normally."
  `(let ((logform nil)
         (retval ,form))

     ;; YOUR MACRO COMPLETION CODE GOES HERE.

     retval))



(define-test test-log-form-and-value
    "Log should start out empty."
  (assert-equal nil *log-with-value*)
    "LOG-FORM-WITH-VALUE does not interfere with the usual return value."
  (assert-equal 1978 (log-form-with-value (* 2 23 43)))
    "LOG-FORM-WITH-VALUE records the code which it has been passed."
  (assert-equal 1 (length *log-with-value*))
  (assert-equal '(:form (* 2 23 43) :value 1978) (first *log-with-value*))
    "Macros evaluating to more macros is OK, if confusing."
  (assert-equal 35 (log-form-with-value (log-form-with-value (- 2013 1978))))
  (assert-equal 3 (length *log-with-value*))
  (assert-equal '(:form (log-form-with-value (- 2013 1978)) :value 35) (first *log-with-value*))
  (assert-equal '(:form (- 2013 1978) :value 35) (second *log-with-value*)))
