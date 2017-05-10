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


; Concept: What do you do to go through the Lisp koans?  You fill in
; the blanks, or otherwise fix the Lisp code so that the code within
; the DEFINE-TEST blocks passes.

  ; In common lisp, "True" and "False" are represented by T and NIL.
  ; More in a future lesson, but for now consider T to be true,
  ; and NIL to be false.
(define-test assert-true
    "T is true.  Replace the blank with a T (or lowercase t)."
  (assert-true ___))

(define-test assert-false
    "NIL (or lowercase nil) is false."
  (assert-false ___))

(define-test fill-in-the-blank
    "Sometimes you will need to fill the blank to complete."
  (assert-equal 2 ___))

(define-test fill-in-the-blank-string
  (assert-equal ___ "hello world"))

(define-test test-true-or-false
    "Sometimes you will be asked to evaluate whether statements
     are true (T) or false (NIL)."
  (true-or-false? ___ (equal 34 34))
  (true-or-false? ___ (equal 19 78)))
