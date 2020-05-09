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

;;;                       ╭╮         ╭╮ ///////
;;;                       ┃┃         ┃┃///////
;;;                       ┃┃╭┳━━┳━━╮ ┃┃╭┳━━┳━━┳━╮╭━━╮
;;;                       ┃┃┣┫━━┫╭╮┃ ┃╰╯┫╭╮┃╭╮┃╭╮┫━━┫
;;;                       ┃╰┫┣━━┃╰╯┃ ┃╭╮┫╰╯┃╭╮┃┃┃┣━━┃
;;;                       ╰━┻┻━━┫╭━╯/╰╯╰┻━━┻╯╰┻╯╰┻━━╯
;;;                             ┃┃ //////
;;;                             ╰╯//////

;;; Welcome to the Lisp Koans.
;;; May the code stored here influence your enlightenment as a programmer.

;;; In order to progress, fill in the blanks, denoted via ____ in source code.
;;; Sometimes, you will be asked to provide values that are equal to something.

(define-test fill-in-the-blanks
  (assert-equal 2 2)
  (assert-equal 3.14 3.14)
  (assert-equal "Hello World" "Hello World"))

;;; Sometimes, you will be asked to say whether something is true or false,
;;; In Common Lisp, the canonical values for truth and falsehood are T and NIL.

(define-test assert-true
  (assert-true t))

(define-test assert-false
  (assert-false nil))

(define-test true-or-false
  (true-or-false? t (= 34 34))
  (true-or-false? nil (= 19 78)))

;;; Since T and NIL are symbols, you can type them in lowercase or uppercase;
;;; by default, Common Lisp will automatically upcase them upon reading.

(define-test upcase-downcase
  ;; Try inserting a lowercase t here.
  (assert-equal t T)
  ;; Try inserting an uppercase NIL here.
  (assert-equal NIL nil))

;;; Sometimes, you will be asked to provide a part of an expression that must be
;;; either true or false.

(define-test a-true-assertion
  (assert-true (= 4 (+ 2 2))))

(define-test a-false-assertion
  (assert-false (= 5 (+ 2 2))))

