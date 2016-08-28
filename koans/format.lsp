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


;; FORMAT is lisp's counterpart to the c function printf. Refer to
;; http://www.gigamonkeys.com/book/a-few-format-recipes.html for more
;; on this topic.


;; FORMAT takes two fixed parameters. The first one specifies an
;; output stream that the result goes to, and if left as nil, FORMAT
;; will return the output as a string instead. The second parameter
;; specifies the format, where format specifier will be replaced by
;; formatting the rest of the parameters.

(define-test test-format-with-plain-text
  "If there is no format specifier, FORMAT just returns the string
   itself."
  (assert-equal "this is plain text." (format nil "this is plain text.")))

(define-test test-format-with-general-specifier
  "~a is a general specifier that translates to the print form of a
    parameter."
  (assert-equal "42" (format nil "~a" 42))
  (assert-equal "C" (format nil "~a" #\C))
  (assert-equal "galaxy far far away" (format nil "~a" "galaxy far far away"))
  ;; ~a can also translate to list
  ;; and parameters to FORMAT are passed by value
  (assert-equal "(/ 8 (- 3 (/ 8 3))) evaluates to 24"
                (format nil "~a evaluates to ~a"
                        '(/ 8 (- 3 (/ 8 3)))
                        (/ 8 (- 3 (/ 8 3))))))

(define-test some-fancy-specifiers
  "format enclosed by ~{ and ~} applies to every element in a list."
  (assert-equal "[1][2][3][4]"
                (format nil "~{[~a]~}" '(1 2 3 4)))
  ;; ~^ within the ~{ ~} stops processing the last element in the list.
  (assert-equal "1|2|3|4|" (format nil "~{~a|~}" '(1 2 3 4)))
  (assert-equal "1|2|3|4" (format nil "~{~a~^|~}" '(1 2 3 4)))
  ;; ~r reads the integer
  (assert-equal "forty-two" (format nil "~r" 42))
  ;; put them all together
  (assert-equal "one,two,three,four"
                (format nil "~{~r~^,~}" '(1 2 3 4))))
