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

(defpackage #:lisp-koans.core
  (:use #:common-lisp
        #:lisp-koans.test)
  (:export #:main))

(in-package :lisp-koans.core)

(defvar *all-koan-groups*
  (with-open-file (in #p".koans")
    (with-standard-io-syntax (read in))))

(defvar *collected-results* nil)

;;; Functions for loading koans

(defun package-name-from-group-name (group-name)
  (format nil "LISP-KOANS.KOANS.~A" group-name))

(defun load-koan-group-named (dirname koan-group-name)
  (let* ((koan-name (string-downcase (string koan-group-name)))
         (koan-file-name (concatenate 'string koan-name ".lisp"))
         (koan-package-name (package-name-from-group-name koan-group-name)))
    (unless (find-package koan-package-name)
      (make-package koan-package-name
                    :use '(#:common-lisp #:lisp-koans.test)))
    (let ((*package* (find-package koan-package-name)))
      (load (concatenate 'string dirname "/" koan-file-name)))))

(defun load-all-koans (dirname)
  (loop for koan-group-name in *all-koan-groups*
        do (load-koan-group-named dirname koan-group-name)))

;;; Functions for executing koans

(defun execute-koans ()
  (loop for koan-group-name in *all-koan-groups*
        for package-name = (package-name-from-group-name koan-group-name)
        for kg-results = (run-koans package-name)
        collect (list koan-group-name kg-results) into results
        do (print-koan-group-progress koan-group-name kg-results)
        while (every (lambda (x) (eq x :pass)) (second (first kg-results)))
        finally (setf *collected-results* results)))

;;; Functions for printing progress

(defun print-koan-group-progress (name results)
  (format t "~%Thinking about ~A~%" name)
  (dolist (result (reverse results))
    (destructuring-bind (test-name results) result
      (let ((format-control (if (every (lambda (x) (equalp :pass x)) results)
                                "    [32m~A has expanded your awareness.~%[0m"
                                "    [31m~A requires more meditation.~%[0m")))
        (format t format-control test-name)))))

;;; Functions for processing results

(defun n-passed-koans-overall (collected-results)
  (flet ((all-asserts-passed-in-koan-p (result)
           (every (lambda (x) (eq :pass x)) (second result))))
    (loop for kg in collected-results
          sum (count-if #'all-asserts-passed-in-koan-p (second kg)))))

(defun any-assert-non-pass-p ()
  (dolist (k-group-result *collected-results*)
    (dolist (result (second k-group-result))
      (dolist (one-assert (second result))
        (when (not (equal one-assert :pass))
          (return-from any-assert-non-pass-p one-assert))))))

;;; Functions for printing results

(defun koan-status-message (koan-status)
  (cond ((find :incomplete koan-status) "[1m[33mA koan is incomplete.[0m")
        ((find :fail koan-status) "[1m[31mA koan is incorrect.[0m")
        ((find :error koan-status) "[1m[31mA koan signaled an error.[0m")
        (t (format nil "[1mLast koan status: ~A.[0m" koan-status))))

(defun print-next-suggestion-message (dirname)
  (let ((filename (caar (last *collected-results*)))
        (koan-name (caaadr (car (last (last *collected-results*)))))
        (koan-status (reverse (cadaar (cdar (last (last *collected-results*)))))))
    (format t "~&You have not yet reached enlightenment.
    ~A
[1mPlease meditate on the following code:[0m
    File \"~A/~(~A~).lisp\"
    Koan \"~A\"
    Current koan assert status is \"~A\"~%~%"
            (koan-status-message koan-status) dirname filename koan-name koan-status)))

(defun print-completion-message ()
  (format t "
*********************************************************
That was the last one, well done! ENLIGHTENMENT IS YOURS!
*********************************************************

If you demand greater challenge, take a look at extra-credit.lisp
Or, let the student become the teacher:
Write and submit your own improvements to https://github.com/google/lisp-koans!~%
"))

(defun print-progress-message ()
  (format t "You are now ~A/~A koans and ~A/~A lessons closer to reaching enlightenment.~%~%"
          (n-passed-koans-overall *collected-results*)
          (test-total-count)
          (1- (length *collected-results*))
          (length *all-koan-groups*)))

(defun output-advice (dirname)
  (cond ((any-assert-non-pass-p)
         (print-next-suggestion-message dirname)
         (print-progress-message))
        (t (print-completion-message))))

;;; Main

(defun main (&optional (dirname "koans"))
  (load-all-koans dirname)
  (execute-koans)
  (output-advice dirname))
