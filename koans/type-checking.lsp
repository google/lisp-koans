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
   (true-or-false? t  (typep "hello" 'string))
   (true-or-false? t  (typep "hello" 'array))
   (true-or-false? nil  (typep "hello" 'list))
   (true-or-false? t  (typep "hello" '(simple-array character (5))))

   (true-or-false? t  (typep '(1 2 3) 'list))
   (true-or-false? t  (typep 99 'integer))
   (true-or-false? t  (typep nil 'NULL))
   (true-or-false? t  (typep 22/7 'ratio))
   (true-or-false? t  (typep 4.0 'float))
   (true-or-false? t  (typep #\a 'character))
   (true-or-false? t  (typep #'length 'function)))


(define-test test-get-type-with-type-of
   (assert-equal 'NULL (type-of ()))
   (assert-equal 'ratio (type-of 4/6)))

(define-test test-type-sets-may-overlap
   (true-or-false? t (typep () 'list))
   (true-or-false? t (typep () 'atom))
   (true-or-false? t (typep () 'NULL))
   (true-or-false? t (typep () t)))


(define-test test-integers-can-get-really-big
   (true-or-false? t (typep 12345678901234567890123456789012 'integer))
   ;; Integers are either fixnum or bignum.
   ;; The boundary between fixnum and bignum is given by the constant:
   ;;   most-positive-fixnum
   (assert-true t (typep 1234567890123456789 'fixnum))
   (assert-true t (typep 12345678901234567890 'bignum))
   (true-or-false? t (typep most-positive-fixnum 'fixnum))
   (true-or-false? nil (typep (+ 1 most-positive-fixnum) 'fixnum)))


(define-test test-lisp-type-system-is-hierarchy
   (assert-true (typep 1 'bit))
   (assert-true (typep 1 'integer))
   (assert-true (typep 2 'integer))
   (true-or-false? t (subtypep 'bit 'integer))
   (true-or-false? t (subtypep (type-of 1) (type-of 2)))
   (true-or-false? nil (subtypep (type-of 2) (type-of 1))))


(define-test test-some-types-are-lists
   (assert-true t (typep (make-array 0 :element-type 'integer) '(SIMPLE-VECTOR 0)))
   (true-or-false? t (typep (make-array '(3 3) :element-type 'integer) '(SIMPLE-ARRAY T (3 3)))))


(define-test test-type-specifier-lists-also-have-hierarchy
   (true-or-false? t (subtypep '(SIMPLE-ARRAY T (3 3)) '(SIMPLE-ARRAY T *)))
   (true-or-false? t (subtypep '(vector double-float 100) '(vector * 100)))
   (true-or-false? t (subtypep '(vector double-float 100) '(vector double-float *)))
   (true-or-false? t (subtypep '(vector double-float 100) '(vector * *)))
   (true-or-false? nil (subtypep '(vector double-float 100) '(array number *)))
   (true-or-false? t (subtypep '(vector double-float 100) t)))


(define-test test-type-coersion
   (assert-true (typep 0 'integer))
   (true-or-false? nil (typep 0 'short-float))
   (true-or-false? nil (subtypep 'integer 'short-float))
   (true-or-false? nil (subtypep 'short-float 'integer))
   (true-or-false? t (typep (coerce 0 'short-float) 'short-float)))


(define-test test-atoms-are-anything-thats-not-a-cons
  (true-or-false? t (atom 4))
  (true-or-false? nil (atom '(1 2 3 4)))
  (true-or-false? t (atom 'some-unbound-name))
  (assert-true t (typep (make-array '(4 4)) '(SIMPLE-ARRAY * *)))
  (true-or-false? t (atom (make-array '(4 4)))))


(define-test test-functionp
    "the functionp predicate is true iff the argument is a function"
  (assert-true (functionp (lambda (a b c) (+ a b c))))
  (true-or-false? t (functionp #'make-array))
  (true-or-false? nil (functionp '(1 2 3)))
  (true-or-false? nil (functionp t)))


(define-test test-there-are-some-other-type-predicates
  ; see http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node73.html for more.
  (true-or-false? t (numberp 999))
  (true-or-false? t (listp '(9 9 9)))
  (true-or-false? t (integerp 999))
  (true-or-false? t (rationalp 9/99))
  (true-or-false? t (floatp 9.99))
  (true-or-false? t (stringp "nine nine nine"))
  (true-or-false? t (characterp #\9))
  (true-or-false? t (bit-vector-p #*01001)))


;; 目前对subtypep不了解
(define-test test-guess-that-type!
    (let ((x '(SIMPLE-ARRAY array (5 3 *))))
      (assert-true (subtypep  x '(SIMPLE-ARRAY T (* 3 *))))
      (assert-true (subtypep  x '(SIMPLE-ARRAY T (5 * *))))
      (assert-true (subtypep  x '(SIMPLE-ARRAY ARRAY *)))
      (assert-true (typep (make-array '(5 3 9) :element-type 'STRING ) x))
      (assert-true (typep (make-array '(5 3 33) :element-type 'VECTOR ) x))))
