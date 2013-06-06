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


(in-package :cl-user)

;; lisp-unit defines the modules for loading / executing koans
(load "lisp-unit.lsp")

(defpackage :lisp-koans
  (:use :common-lisp)
  (:use :lisp-unit)
  #+sbcl (:use :sb-ext))

(in-package :lisp-koans)

;; .koans file controls which files in *koan-dir-name* are loaded as
;; koans to complete
(defvar *koan-dir-name* "koans")
(with-open-file (in #P".koans")
  (with-standard-io-syntax
    (defvar *all-koans-groups* (read in))))

;; set *print-koan-progress* to t to list all completed koans before summary
(defvar *print-koan-progress* t)
;; debug-print directives
(defvar *dp-loading* nil)


;; Global state used to hold results of loading and processing koans
(defvar *n-total-koans* 0)
(defvar *collected-results* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for loading koans ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-koan-group-named (koan-group-name)
  ;; Creates a package for the koan-group based on koan-group-name.
  ;; Loads a lisp file at *koan-dir-name* / koan-group-name .lsp
  ;; Adds all the koans from that file to the package.
  (let ((koan-file-name (concatenate 'string (string-downcase (string koan-group-name)) ".lsp")))
    (if *dp-loading* (format t "start loading ~A ~%" koan-file-name))
    (in-package :lisp-koans)
    (unless (find-package koan-group-name)
      (make-package koan-group-name
                    :use '(:common-lisp :lisp-unit #+sbcl :sb-ext)))
    (setf *package* (find-package koan-group-name))
    (load (concatenate 'string *koan-dir-name* "/" koan-file-name))
    (incf *n-total-koans* (length (list-tests)))
    (in-package :lisp-koans)
    (if *dp-loading* (format t "done loading ~A ~%" koan-file-name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for executing koans ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-koan-group-named (koan-group-name)
  ;; Executes the koan group, using run-koans defined in lisp-unit
  ;; returning a test-results object.
  (if *dp-loading* (format t "start running ~A ~%" koan-group-name))
  (run-koans koan-group-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for printing progress ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-one-koan-status (k-result)
  (let ((koan-name (first k-result))
        (all-pass-p (every
                     #'(lambda (x) (equalp :pass x))
                     (second k-result))))
    (if all-pass-p
        (format t "~A has expanded your awareness.~%" koan-name)
        (format t "~A requires more meditation.~%" koan-name))))

(defun print-koan-group-progress (kg-name kg-results)
  (format t "~%Thinking about ~A~%" kg-name)
  (dolist (k-result (reverse kg-results))
    (format t "    ")
    (print-one-koan-status k-result))
  (format t "~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for processing results ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun any-assert-non-pass-p ()
   (dolist (k-group-result *collected-results*)
     (dolist (koan-result (second k-group-result))
       (dolist (one-assert (second koan-result))
         (if (not (equal one-assert :pass))
             (return-from any-assert-non-pass-p one-assert)))))
   nil)

(defun get-error-filename (collected-results)
  (first (first (last collected-results))))

(defun get-error-koan-name (collected-results)
  (first (first (second (first (last (last collected-results)))))))

(defun get-error-koan-status (collected-results)
  (second (first (second (first (last (last collected-results)))))))

(defun koan-status-message (koan-status)
  (if (find :incomplete koan-status)
       (return-from koan-status-message
         "  A koan is incomplete.~%"))
  (if (find :fail koan-status)
       (return-from koan-status-message
         "  A koan is incorrect.~%"))
  (if (find :error koan-status)
       (return-from koan-status-message
         "  A koan threw an error.~%"))
  (format t "  last koan status: ~A~%" koan-status)
  "")

(defun print-next-suggestion-message ()
  (let ((filename (get-error-filename *collected-results*))
        (koan-name (get-error-koan-name *collected-results*))
        (koan-status (get-error-koan-status *collected-results*)))
    (format t "You have not yet reached enlightenment ...~%")
    (format t (koan-status-message koan-status))
    (format t "~%")
    (format t "Please meditate on the following code:~%")
    (format t "   File \"~A/~A.lsp\"~%" *koan-dir-name* (string-downcase filename))
    (format t "   Koan \"~A\"~%" koan-name)
    (format t "   Current koan assert status is \"~A\"~%" (reverse koan-status))))

(defun print-completion-message ()
  (format t "**********************************************************~%")
  (format t "That was the last one, well done!  ENLIGHTENMENT IS YOURS!~%")
  (format t "**********************************************************~%~%")
  (format t "If you demand greater challenge, take a look at extra-credit.lsp~%")
  (format t "Or, let the student become the teacher:~%")
  (format t "   Write and submit your own improvements to github.com/google/lisp-koans!~%"))

(defun n-completed-koans (collected-results)
  (loop for kg in collected-results
        sum (length (second kg)) into partial-sum
        finally (return partial-sum)))

(defun all-asserts-passed-in-koan-p (koan-result)
  (equal
   (length (second koan-result))
   (count :pass (second koan-result))))

(defun n-passed-koans-in-group (kg)
  (loop for k in (second kg)
        counting (all-asserts-passed-in-koan-p k) into partial-sum
        finally (return partial-sum)))

(defun n-passed-koans-overall (collected-results)
  (loop for kg in collected-results
        sum (n-passed-koans-in-group kg) into partial-sum
        finally (return partial-sum)))

(defun print-progress-message ()
      (format t "You are now ~A/~A koans and ~A/~A lessons closer to reaching enlightenment~%~%"
              (n-passed-koans-overall *collected-results*)
              *n-total-koans*
              (- (length *collected-results*) 1)
              (length *all-koans-groups*)))


;;;;;;;;;;
;; Main ;;
;;;;;;;;;;

;; Load all the koans before testing any, and
;; count how many total koans there are.
(loop for koan-group-name in *all-koans-groups*
      do
   (load-koan-group-named koan-group-name))

;; Run through the koans until reaching the end condition.
;; Store the results in *collected-results*
(setf *collected-results*
      (loop for koan-group-name in *all-koans-groups*
            for kg-results = (run-koan-group-named koan-group-name)
            collect (list koan-group-name kg-results)
            do (if *print-koan-progress*
                   (print-koan-group-progress koan-group-name kg-results))
               ;; *proceed-after-failure* is defined in lisp-unit
            until (and (not *proceed-after-failure*) (any-non-pass-p kg-results))))


;; Output advice to the learner
(if (any-assert-non-pass-p)
    (progn
      (print-next-suggestion-message)
      (format t "~%")
      (print-progress-message))
    (print-completion-message))
