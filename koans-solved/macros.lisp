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

;;; A Lisp macro is a function that accepts Lisp data and produces a Lisp form.
;;; When the macro is called, its macro function receives unevaluated arguments
;;; and may use them to produce a new Lisp form. This form is then spliced in
;;; place of the original macro call and is then evaluated.

(defmacro my-and (&rest forms)
  ;; We use a LABELS local function to allow for recursive expansion.
  (labels ((generate (forms)
             (cond ((null forms) 'nil)
                   ((null (rest forms)) (first forms))
                   (t `(when ,(first forms)
                         ,(generate (rest forms)))))))
    (generate forms)))

(define-test my-and
  ;; ASSERT-EXPANDS macroexpands the first form once and checks if it is equal
  ;; to the second form.
  (assert-expands (my-and (= 0 (random 6)) (error "Bang!"))
                  '(when (= 0 (random 6)) (error "Bang!")))
  (assert-expands (my-and (= 0 (random 6))
                          (= 0 (random 6))
                          (= 0 (random 6))
                          (error "Bang!"))
                  '(when (= 0 (random 6))
                    (when (= 0 (random 6))
                      (when (= 0 (random 6))
                        (error "Bang!"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A common macro pitfall is capturing a variable defined by the user.

(define-test variable-capture
  (macrolet ((for ((var start stop) &body body)
               `(do ((,var ,start (1+ ,var))
                     (limit ,stop))
                    ((> ,var limit))
                  ,@body)))
    (let ((limit 10)
          (result '()))
      (for (i 0 3)
           (push i result)
           (assert-equal 3 limit))
      (assert-equal '(0 1 2 3) (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Another pitfall is evaluating some forms multiple times where they are only
;;; meant to be evaluated once.

(define-test multiple-evaluation
  ;; We use MACROLET for defining a local macro.
  (macrolet ((for ((var start stop) &body body)
               `(do ((,var ,start (1+ ,var)))
                    ((> ,var ,stop))
                  ,@body)))
    (let ((side-effects '())
          (result '()))
      ;; Our functions RETURN-0 and RETURN-3 have side effects.
      (flet ((return-0 () (push 0 side-effects) 0)
             (return-3 () (push 3 side-effects) 3))
        (for (i (return-0) (return-3))
             (push i result)))
      (assert-equal '(0 1 2 3) (nreverse result))
      (assert-equal '(0 3 3 3 3 3) (nreverse side-effects)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Yet another pitfall is not respecting the evaluation order of the macro
;;; subforms.

(define-test wrong-evaluation-order
  (macrolet ((for ((var start stop) &body body)
               ;; The function GENSYM creates GENerated SYMbols, guaranteed to
               ;; be unique in the whole Lisp system. Because of that, they
               ;; cannot capture other symbols, preventing variable capture.
               (let ((limit (gensym "LIMIT")))
                 `(do ((,limit ,stop)
                       (,var ,start (1+ ,var)))
                      ((> ,var ,limit))
                    ,@body))))
    (let ((side-effects '())
          (result '()))
      (flet ((return-0 () (push 0 side-effects) 0)
             (return-3 () (push 3 side-effects) 3))
        (for (i (return-0) (return-3))
             (push i result)))
      (assert-equal '(0 1 2 3) (nreverse result))
      (assert-equal '(3 0) (nreverse side-effects)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test for
  (macrolet ((for ((var start stop) &body body)
               ;; Fill in the blank with a correct FOR macroexpansion that is
               ;; not affected by the three macro pitfalls mentioned above.
               (let ((limit (gensym "LIMIT")))
                 `(do ((,var ,start (1+ ,var))
                       (,limit ,stop))
                      ((> ,var ,limit))
                    ,@body))))
    (let ((side-effects '())
          (result '()))
      (flet ((return-0 () (push 0 side-effects) 0)
             (return-3 () (push 3 side-effects) 3))
        (for (i (return-0) (return-3))
             (push i result)))
      (assert-equal '(0 1 2 3) (nreverse result))
      (assert-equal '(0 3) (nreverse side-effects)))))
