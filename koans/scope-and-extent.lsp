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


(defun shadow-z (z)
;; reuses the symbol name z to build a return value
;; returns a list like (value-of-z, 2) 
  (cons z
        (cons (let ((z 2)) z)
              nil)))

(define-test test-shadowing-a-variable
  (assert-equal '(1 2) (shadow-z 1)))


(defun code-block-01 ()
;; illustrates a basic property of code-blocks
  (block here
    (return-from here 4)
    5))

(defun code-block-02 ()
  (block outer
    (block inner
      (return-from outer 'space)
      (return-from inner 'tube))
    (return-from outer 'valve)))

(define-test test-code-block-01
  (assert-equal 4 (code-block-01)))

(define-test test-code-block-02
  (assert-equal 'space (code-block-02)))


;; About closures and the distinction of lexical and dynamic bindings

;; this recipe from stackoverflow
;; http://stackoverflow.com/questions/463463/dynamic-and-lexical-variables-in-common-lisp
; (print "no special x: a typical closure.")

;; bind f to a function which depends on a local variable x
;; then invoke f to see which value of x is returned.

(define-test test-lexical-bindings-may-be-shadowed
  (assert-eq 10 (let ((f (let ((x 10))
                 (lambda () x))))  ; <-- x bound lexically
    (let ((x 20))          ; form 2
      (funcall f)))))


(define-test test-special-bindings-look-back-on-execution-path
  (assert-eq 20 (let ((f (let ((x 10))
             (declare (special x))
             (lambda () x))))      ; <-- x bound dynamically
    (let ((x 20))          ; form 2
      (declare (special x))
    (funcall f)))))
