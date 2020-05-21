;;; Copyright 2013 Google Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Lisp condition types are very similar to classes. The standard specifies
;;; multiple standard condition types: among them, CONDITION, WARNING,
;;; SERIOUS-CONDITION, and ERROR.

;;; The type CONDITION is the base type of all condition objects.

(define-condition my-condition () ())

;;; The type WARNING is the base type of all conditions of which the programmer
;;; should be warned, unless the condition is somehow handled by the program.

(define-condition my-warning (warning) ())

;;; The type SERIOUS-CONDITION includes programming errors and other situations
;;; where computation cannot proceed (e.g. due to memory or storage issues).

(define-condition my-serious-condition (serious-condition) ())

;;; The type ERROR is the base type for all error situations in code.

(define-condition my-error (error) ())

(define-test type-hierarchy
  ;; Inheritance for condition types works the same way as for classes.
  (let ((condition (make-condition 'my-condition)))
    (true-or-false? ____ (typep condition 'my-condition))
    (true-or-false? ____ (typep condition 'condition))
    (true-or-false? ____ (typep condition 'warning))
    (true-or-false? ____ (typep condition 'error)))
  (let ((condition (make-condition 'my-warning)))
    (true-or-false? ____ (typep condition 'my-warning))
    (true-or-false? ____ (typep condition 'warning))
    (true-or-false? ____ (typep condition 'error)))
  (let ((condition (make-condition 'my-serious-condition)))
    (true-or-false? ____ (typep condition 'my-serious-condition))
    (true-or-false? ____ (typep condition 'serious-condition))
    (true-or-false? ____ (typep condition 'warning))
    (true-or-false? ____ (typep condition 'error)))
  (let ((condition (make-condition 'my-error)))
    (true-or-false? ____ (typep condition 'my-error))
    (true-or-false? ____ (typep condition 'my-serious-condition))
    (true-or-false? ____ (typep condition 'serious-condition))
    (true-or-false? ____ (typep condition 'warning))
    (true-or-false? ____ (typep condition 'error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A condition handler is composed of a handler function that accepts a
;;; condition object and a condition type for which the function will be called.

(defvar *list*)

(define-condition silly-condition () ())

(define-condition very-silly-condition (silly-condition) ())

(define-condition most-silly-condition (very-silly-condition) ())

(defun handle-silly-condition (condition)
  (declare (ignore condition))
  (push :silly-condition *list*))

(defun handle-very-silly-condition (condition)
  (declare (ignore condition))
  (push :very-silly-condition *list*))

(defun handle-most-silly-condition (condition)
  (declare (ignore condition))
  (push :most-silly-condition *list*))

(define-test handler-bind
  ;; When a condition is signaled, all handlers whose type matches the
  ;; condition's type are allowed to execute.
  (let ((*list* '()))
    (handler-bind ((very-silly-condition #'handle-very-silly-condition)
                   (silly-condition #'handle-silly-condition)
                   (most-silly-condition #'handle-most-silly-condition))
      (signal (make-condition 'most-silly-condition)))
    (assert-equal ____ *list*)))

(define-test handler-order
  ;; The order of binding handlers matters.
  (let ((*list* '()))
    (handler-bind ((silly-condition #'handle-silly-condition)
                   (very-silly-condition #'handle-very-silly-condition)
                   (most-silly-condition #'handle-most-silly-condition))
      (signal (make-condition 'most-silly-condition)))
    (assert-equal ____ *list*)))

(define-test multiple-handler-binds
  ;; It is possible to bind handlers in steps.
  (let ((*list* '()))
    (handler-bind ((silly-condition #'handle-silly-condition)
                   (most-silly-condition #'handle-most-silly-condition))
      (handler-bind ((very-silly-condition #'handle-very-silly-condition))
        (signal (make-condition 'most-silly-condition))))
    (assert-equal ____ *list*)))

(define-test same-handler
  ;; The same handler may be bound multiple times.
  (let ((*list* '()))
    (handler-bind ((silly-condition #'handle-silly-condition)
                   (silly-condition #'handle-silly-condition))
      (handler-bind ((very-silly-condition #'handle-very-silly-condition)
                     (silly-condition #'handle-silly-condition)
                     (very-silly-condition #'handle-very-silly-condition))
        (signal (make-condition 'most-silly-condition))))
    (assert-equal ____ *list*)))

(define-test handler-types
  ;; A handler is not executed if it does not match the condition type.
  (let ((*list* '()))
    (handler-bind ((silly-condition #'handle-silly-condition)
                   (very-silly-condition #'handle-very-silly-condition)
                   (most-silly-condition #'handle-most-silly-condition))
      (signal (make-condition 'very-silly-condition)))
    (assert-equal ____ *list*)))

(define-test handler-transfer-of-control
  ;; A handler may decline to handle the condition if it returns normally,
  ;; or it may handle the condition by transferring control elsewhere.
  (let ((*list* '()))
    (block my-block
      (handler-bind ((silly-condition #'handle-silly-condition)
                     (silly-condition (lambda (condition)
                                        (declare (ignore condition))
                                        (return-from my-block)))
                     (silly-condition #'handle-silly-condition))
        (signal (make-condition 'silly-condition))))
    (assert-equal ____ *list*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-error (condition)
  (declare (ignore condition))
  (push :error *list*))

(define-condition my-error (error) ())

(defun handle-my-error (condition)
  (declare (ignore condition))
  (push :my-error *list*))

(define-test handler-case
  ;; HANDLER-CASE always transfers control before executing the case forms.
  (let ((*list* '()))
    (handler-case (signal (make-condition 'my-error))
      (error (condition) (handle-error condition))
      (my-error (condition) (handle-my-error condition)))
    (assert-equal ____ *list*)))

(define-test handler-case-order
  ;; The order of handler cases matters.
  (let ((*list* '()))
    (handler-case (signal (make-condition 'my-error))
      (my-error (condition) (handle-my-error condition))
      (error (condition) (handle-error condition)))
    (assert-equal ____ *list*)))

(define-test handler-case-type
  ;; A handler cases is not executed if it does not match the condition type.
  (let ((*list* '()))
    (handler-case (signal (make-condition 'error))
      (my-error (condition) (handle-my-error condition))
      (error (condition) (handle-error condition)))
    (assert-equal ____ *list*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun divide (numerator denominator)
  (/ numerator denominator))

(define-test error-signaling
  ;; ASSERT-ERROR is a Lisp Koans macro which verifies that the correct error
  ;; type is signaled.
  (assert-equal 3 (divide 6 2))
  (assert-error (divide 6 0) 'division-by-zero)
  (assert-error (divide 6 :zero) 'type-error))

(define-test error-signaling-handler-case
  (flet ((try-to-divide (numerator denominator)
           ;; In code outside Lisp Koans, HANDLER-CASE should be used.
           (handler-case (divide numerator denominator)
             (division-by-zero () :division-by-zero)
             (type-error () :type-error))))
    (assert-equal ____ (try-to-divide 6 2))
    (assert-equal ____ (try-to-divide 6 0))
    (assert-equal ____ (try-to-divide 6 :zero))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Condition objects can contain metadata about the specific situation that
;;; occurred in the code.

(define-test accessors-division-by-zero
  (let ((condition (handler-case (divide 6 0) (division-by-zero (c) c))))
    ;; Disabled on CLISP and ABCL due to conformance bugs.
    ;; See https://gitlab.com/gnu-clisp/clisp/-/issues/22
    ;; See https://github.com/armedbear/abcl/issues/177
    #-(or clisp abcl)
    (assert-equal ____ (arithmetic-error-operands condition))
    (let ((operation (arithmetic-error-operation condition)))
      ;; Disabled on ABCL due to a conformance bug.
      ;; See https://github.com/armedbear/abcl/issues/177
      #-abcl
      (assert-equal ____ (funcall operation 12 4)))))

(define-test accessors-type-error
  (let ((condition (handler-case (divide 6 :zero) (type-error (c) c))))
    (assert-equal ____ (type-error-datum condition))
    (let ((expected-type (type-error-expected-type condition)))
      (true-or-false? ____ (typep :zero expected-type))
      (true-or-false? ____ (typep 0 expected-type))
      (true-or-false? ____ (typep "zero" expected-type))
      (true-or-false? ____ (typep 0.0 expected-type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We can define slots in our own condition types in a way that is similar to
;; DEFCLASS.

(define-condition parse-log-line-error (parse-error)
  ((line :initarg :line :reader line)
   (reason :initarg :reason :reader reason)))

(defun log-line-type (line)
  ;; The macro CHECK-TYPE signals a TYPE-ERROR if the object is not of the
  ;; specified type.
  (check-type line string)
  (cond ((eql 0 (search "TIMESTAMP" line)) :timestamp)
        ((eql 0 (search "HTTP" line)) :http)
        ((eql 0 (search "LOGIN" line)) :login)
        ;; The function ERROR should be used for signaling serious conditions
        ;; and errors: if the condition is not handled, it halts program
        ;; execution and starts the Lisp debugger.
        (t (error 'parse-log-line-error :line line
                                        :reason :unknown-log-line-type))))

(define-test log-line-type-errors
  (flet ((try-log-line-type (line)
           (handler-case (log-line-type line)
             (error (condition) condition))))
    (assert-equal ____ (try-log-line-type "TIMESTAMP 2020-05-08 16:59:39"))
    (assert-equal ____ (try-log-line-type "HTTP GET / from 127.0.0.1"))
    (assert-equal ____ (try-log-line-type "LOGIN administrator:hunter2"))
    (let ((condition (try-log-line-type "WARNING: 95% of disk space used")))
      (assert-equal ____ (line condition))
      (assert-equal ____ (reason condition)))
    (let ((condition (try-log-line-type 5555)))
      (assert-equal 'string (____ condition))
      (assert-equal 5555 (____ condition)))))
