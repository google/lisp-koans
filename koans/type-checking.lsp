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

;; Common lisp types have hierarchy.  Any object may belong a family of types.
;; The top level type, which includes everything else, is 't'

(define-test test-check-some-common-types
   (true-or-false? ___  (typep "hello" 'string))
   (true-or-false? ___  (typep "hello" 'array))
   (true-or-false? ___  (typep "hello" 'list))
   (true-or-false? ___  (typep "hello" '(simple-array character (5))))

   (true-or-false? ___  (typep '(1 2 3) 'list))
   (true-or-false? ___  (typep 99 'integer))
   (true-or-false? ___  (typep nil 'NULL))
   (true-or-false? ___  (typep 22/7 'ratio))
   (true-or-false? ___  (typep 4.0 'float))
   (true-or-false? ___  (typep #\a 'character))
   (true-or-false? ___  (typep #'length 'function)))


(define-test test-get-type-with-type-of
   (assert-equal ____ (type-of ()))
   (assert-equal ____ (type-of 4/6)))

(define-test test-type-sets-may-overlap
   (true-or-false? ___  (typep () 'list))
   (true-or-false? ___  (typep () 'atom))
   (true-or-false? ___  (typep () 'NULL))
   (true-or-false? ___  (typep () t)))


(define-test test-integers-can-get-really-big
   (true-or-false? ____ (typep 12345678901234567890123456789012 'integer))
   ;; Integers are either fixnum or bignum.
   ;; The boundary between fixnum and bignum is given by the constant:
   ;;   most-positive-fixnum
   (assert-true (typep 1234567890123456789 'fixnum))
   (assert-true (typep 12345678901234567890 'bignum))
   (true-or-false? ___ (typep most-positive-fixnum 'fixnum))
   (true-or-false? ___ (typep (+ 1 most-positive-fixnum) 'fixnum)))


(define-test test-lisp-type-system-is-hierarchy
   (assert-true (typep 1 'bit))
   (assert-true (typep 1 'integer))
   (assert-true (typep 2 'integer))
   (true-or-false? ____ (subtypep 'bit 'integer))
   (true-or-false? ____ (subtypep (type-of 1) (type-of 2)))
   (true-or-false? ____ (subtypep (type-of 2) (type-of 1))))


(define-test test-some-types-are-lists
   (assert-true(typep (make-array 0 :element-type 'integer) '(SIMPLE-VECTOR 0)))
   (true-or-false? ____ (typep (make-array '(3 3) :element-type 'integer) '(SIMPLE-ARRAY T (3 3)))))


(define-test test-type-specifier-lists-also-have-hierarchy
   (true-or-false? ____ (subtypep '(SIMPLE-ARRAY T (3 3)) '(SIMPLE-ARRAY T *)))
   (true-or-false? ____ (subtypep '(vector double-float 100) '(vector * 100)))
   (true-or-false? ____ (subtypep '(vector double-float 100) '(vector double-float *)))
   (true-or-false? ____ (subtypep '(vector double-float 100) '(vector * *)))
   (true-or-false? ____ (subtypep '(vector double-float 100) '(array number *)))
   (true-or-false? ____ (subtypep '(vector double-float 100) t)))


(define-test test-type-coersion
   (assert-true (typep 0 'integer))
   (true-or-false? ___ (typep 0 'short-float))
   (true-or-false? ___ (subtypep 'integer 'short-float))
   (true-or-false? ___ (subtypep 'short-float 'integer))
   (true-or-false? ___ (typep (coerce 0 'short-float) 'short-float)))


(define-test test-atoms-are-anything-thats-not-a-cons
  (true-or-false? ___ (atom 4))
  (true-or-false? ___ (atom '(1 2 3 4)))
  (true-or-false? ___ (atom 'some-unbound-name))
  (assert-true (typep (make-array '(4 4)) '(SIMPLE-ARRAY * *)))
  (true-or-false? ___ (atom (make-array '(4 4)))))


(define-test test-functionp
    "the functionp predicate is true iff the argument is a function"
  (assert-true (functionp (lambda (a b c) (+ a b c))))
  (true-or-false? ___ (functionp #'make-array))
  (true-or-false? ___ (functionp '(1 2 3)))
  (true-or-false? ___ (functionp t)))


(define-test test-there-are-some-other-type-predicates
  ; see http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node73.html for more.
  (true-or-false? ___ (numberp 999))
  (true-or-false? ___ (listp '(9 9 9)))
  (true-or-false? ___ (integerp 999))
  (true-or-false? ___ (rationalp 9/99))
  (true-or-false? ___ (floatp 9.99))
  (true-or-false? ___ (stringp "nine nine nine"))
  (true-or-false? ___ (characterp #\9))
  (true-or-false? ___ (bit-vector-p #*01001)))


(define-test test-guess-that-type!
    (let ((x ____))
      (assert-true (subtypep  x '(SIMPLE-ARRAY T (* 3 *))))
      (assert-true (subtypep  x '(SIMPLE-ARRAY T (5 * *))))
      (assert-true (subtypep  x '(SIMPLE-ARRAY ARRAY *)))
      (assert-true (typep (make-array '(5 3 9) :element-type 'STRING ) x))
      (assert-true (typep (make-array '(5 3 33) :element-type 'VECTOR ) x))))
