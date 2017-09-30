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


"Common lisp conditions are much like CLOS classes.
They are used to handle exceptional situations, and separate
error handling code from normal operational code."

(define-condition minimal-error-cond (error) ())
(define-condition minimal-warning-cond (warning) ())


(define-test test-conditions-derive-from-types
    "conditions inherit from base types"
  (true-or-false? t (typep (make-condition 'minimal-error-cond)
                             'minimal-error-cond))

  (true-or-false? t (typep (make-condition 'minimal-error-cond)
                             'error))

  (true-or-false? nil (typep (make-condition 'minimal-error-cond)
                             'warning))

  (true-or-false? t (typep (make-condition 'minimal-warning-cond)
                             'minimal-warning-cond))

  (true-or-false? nil (typep (make-condition 'minimal-warning-cond)
                             'error))

  (true-or-false? t (typep (make-condition 'minimal-warning-cond)
                             'warning)))


;; ----


(define-condition my-div-by-zero-error (error) ())
(define-condition my-non-number-args-error (error) ())

(defun my-divide (num denom)
  (if (or (not (numberp num))
          (not (numberp denom)))
      (error 'my-non-number-args-error))
  (if (= 0 denom)
      (error 'my-div-by-zero-error)
      (/ num denom)))

(define-test assert-error-thrown
    "assert-error checks that the right error is thrown"
  (assert-equal 3 (my-divide 6 2))
  (assert-error 'my-div-by-zero-error (my-divide 6 0))
  (assert-error 'my-non-number-args-error (my-divide 6 "zero")))


(define-test test-handle-errors
    "the handler case is like a case statement which can capture errors
     and warnings, and execute appropriate forms in those conditions."
  (assert-equal 3
                (handler-case (my-divide 6 2)
                  (my-div-by-zero-error (condition) :zero-div-error)
                  (my-non-number-args-error (condition) :bad-args)))
  (assert-equal :zero-div-error
                (handler-case (my-divide 6 0)
                  (my-div-by-zero-error (condition) :zero-div-error)
                  (my-non-number-args-error (condition) :bad-args)))
  (assert-equal :bad-args
                (handler-case (my-divide 6 "woops")
                  (my-div-by-zero-error (condition) :zero-div-error)
                  (my-non-number-args-error (condition) :bad-args))))


;; ----

"conditions, as CLOS objects, can have slots, some of which have special
meanings.  Common Lisp the Language Chapter 29 for more details.
http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node312.html"

; This error condition is more than a signal.  It carries data in two slots.
; the "original-line" slot and the "reason" slot.  Both slots have a defined
; :initarg, which they will use to set themselves, if available.  If not,
; they have a default form (:initform).  They also both provide reader functions

(define-condition logline-parse-error (error)
  ((original-line :initarg :original-line :initform "line not given" :reader original-line)
   (reason :initarg :reason :initform "no-reason" :reader reason)))


;; This function is designed to take loglines, and report what type they are.
;; It can also throw errors, like div-by-zero above, but the errors now carry some
;;  additional information carried within the error itself.

(defun get-logline-type (in-line)
  (if (not (typep in-line 'string))
      ;; if the in-line isn't a string, throw a logline-parse-error, and set the :reason and :original-line
      (error 'logline-parse-error :original-line in-line :reason :bad-type-reason))
  (cond
    ((equal 0 (search "TIMESTAMP" in-line)) :timestamp-logline-type)
    ((if (equal 0 (search "HTTP" in-line)) :http-logline-type))
    ;; if we don't recognize the first token,  throw a logline-parse-error, and set the :reason and :original-line
    (t (error 'logline-parse-error :original-line in-line :reason :unknown-token-reason))))


(define-test test-errors-have-slots
    (assert-equal :timestamp-logline-type
                  (handler-case (get-logline-type "TIMESTAMP y13m01d03")
                    (logline-parse-error (condition) (list (reason condition) (original-line condition)))))
    (assert-equal :http-logline-type
                  (handler-case (get-logline-type "HTTP access 128.0.0.100")
                    (logline-parse-error (condition) (list (reason condition) (original-line condition)))))
    (assert-equal '(:unknown-token-reason "bogus logline")
                  (handler-case (get-logline-type "bogus logline")
                    (logline-parse-error (condition) (list (reason condition) (original-line condition)))))
    (assert-equal '(:bad-type-reason 5555)
                  (handler-case (get-logline-type 5555)
                    (logline-parse-error (condition) (list (reason condition) (original-line condition))))))
