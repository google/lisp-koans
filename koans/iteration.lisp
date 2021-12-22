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

;;; Lisp has multiple options for iteration.
;;; This set of koans will introduce some of the most common ones.

(define-test dolist
  (let ((numbers '(4 8 15 16 23 42)))
    ;; The macro DOLIST binds a variable to subsequent elements of a list.
    (let ((sum 0))
      (dolist (number numbers)
        ;; (INCF PLACE N) is equivalent to (SETF PLACE (+ N PLACE)).
        (incf sum number))
      (assert-equal ____ sum))
    ;; DOLIST can optionally return a value.
    (let ((sum 0))
      (assert-equal ____ (dolist (number numbers sum)
                           (incf sum number))))))

(define-test dotimes
  ;; The macro DOTIMES binds a variable to subsequent integers from 0 to
  ;; (1- COUNT).
  (let ((stack '()))
    (dotimes (i 5)
      (push i stack))
    (assert-equal ____ stack))
  ;; DOTIMES can optionally return a value.
  (let ((stack '()))
    (assert-equal ____ (dotimes (i 5 stack)
                         (push i stack)))))

(define-test do
  ;; The macro DO accepts a list of variable bindings, a termination test with
  ;; epilogue forms, and Lisp code that should be executed on each iteration.
  (let ((result '()))
    (do ((i 0 (1+ i)))
        ((> i 5))
      (push i result))
    (assert-equal ____ result))
  ;; The epilogue of DO can return a value.
  (let ((result (do ((i 0 (1+ i))
                     ;; A variable bound by DO does not need to be updated on
                     ;; each iteration.
                     (result '()))
                    ((> i 5) (nreverse result))
                  (push i result))))
    (assert-equal ____ result)))

(define-test loop-basic-form
  ;; The macro LOOP in its simple form loops forever. It is possible to stop the
  ;; looping by calling the RETURN special form.
  (let ((counter 0))
    (loop (incf counter)
          (when (>= counter 100)
            (return counter)))
    (assert-equal ____ counter))
  ;; The RETURN special form can return a value out of a LOOP.
  (let ((counter 0))
    (assert-equal ____ (loop (incf counter)
                             (when (>= counter 100)
                               (return counter)))))
  ;; The extended form of LOOP will be contemplated in a future koan.
  )

