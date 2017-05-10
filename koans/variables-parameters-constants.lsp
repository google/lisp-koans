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

(defun test-variable-assignment-with-setf ()
    ; The LET pattern allows us to create local variables with
    ; lexical scope.
  (let (var_name_1 (var_name_2 "Michael"))
    ; Variables may be defined with or without initial values.
  (and
    (equalp var_name_2 "Michael")
      ; New values may be assigned to variables with SETF.
    (setf var_name_2 "Janet")
    (equalp var_name_2 "Janet")
      ; SETF may assign multiple variables in one form.
    (setf var_name_1 "Tito"
          var_name_2 "Jermaine")
    (equalp var_name_1 "Tito")
    (equalp var_name_2 "Jermaine"))))

(defun test-setf-for-lists ()
    ; SETF also works on list elements.
  (let (l)
    (setf l '(1 2 3))
    (equalp l '(1 2 3))
      ; FIRST, SECOND, and THIRD are convenient accessor functions
      ; referring to the elements of a list.
      ; For those interested, they are equivalent to CAR, CADR, and CADDR.
    (setf (first l) 10)
    (setf (second l) 20)
    (setf (third l) 30)
    (equalp l '(10 20 30))))

(defparameter param_name_1 "Janet")
  ; DEFPARAMETER requires an initial form.  It is a compiler error to exclude it.
  ;; (defparameter param_no_init) ;; This will fail
(defconstant additive_identity 0)
  ; DEFCONSTANT also requires an initial form.
  ;; (defconstant constant_no_init) ;; This will fail

  ; Reassigning parameters to new values is also OK, but parameters carry the
  ; connotation of immutability.  If it's going to change frequently, it should
  ; be a var.
(setf param_name_1 "The other one")

  ; Reassigning a constant is an error.
  ;; (setf additive_identity -1) ;; This should result in a compile time error.

  ; -------------------------------
  ; Below is necessary to run tests.
  ; -------------------------------

(defvar failed-test-names nil)

(defun run-test (testfun)
  (let ((fun-name (function-name testfun)))
    (if (apply testfun '())
        (format t ".")
        (progn
          (setf failed-test-names (cons fun-name failed-test-names))
          (format t "F")))))

(defun function-name (function) (nth-value 2 (function-lambda-expression function)))


(run-test #'test-variable-assignment-with-setf)
(run-test #'test-setf-for-lists)

(format t "~%")

(defun report-failure (test-name)
  (format t "~S failed.~%" test-name))

(if (endp failed-test-names)  ; No failed tests.
    (format t "all tests pass.~%")
    (mapcar #'report-failure failed-test-names))
