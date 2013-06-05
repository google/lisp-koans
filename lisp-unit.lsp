;;;-*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

#|
This version of lisp-unit.lsp has been extended to support the lisp koans.
Specifically, it is an unnamed branch from
https://github.com/OdonataResearchLLC/lisp-unit/
with hash 93d07b2fa6e32364916225f6218e9e7313027c1f

Modifications were made to:
   1) Support *incomplete* tests in addition to *passing* and *failing* ones
   2) End test execution at the first non-passing test.
|#


#|
Copyright (c) 2004-2005 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.


How to use
----------

1. Read the documentation at:
   https://github.com/OdonataResearchLLC/lisp-unit/wiki

2. Make a file of DEFINE-TEST's. See exercise-tests.lisp for many
examples. If you want, start your test file with (REMOVE-TESTS :ALL)
to clear any previously defined tests.

3. Load this file.

4. (use-package :lisp-unit)

5. Load your code file and your file of tests.

6. Test your code with (RUN-TESTS '(test-name1 test-name2 ...)) or
simply (RUN-TESTS :ALL) to run all defined tests.

A summary of how many tests passed and failed will be printed.

NOTE: Nothing is compiled until RUN-TESTS is expanded. Redefining
functions or even macros does not require reloading any tests.

|#

#|
   Copyright 2013 Google Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
|#

;;; Packages
(in-package :cl-user)

(defpackage :lisp-unit
  (:use :common-lisp)
  ;; Print parameters
  (:export :*print-summary*
           :*print-failures*
           :*print-errors*
           :*proceed-after-failure*)
  ;; Forms for assertions
  (:export :assert-eq
           :assert-eql
           :assert-equal
           :true-or-false?
           :assert-equalp
           :assert-equality
           :assert-prints
           :assert-expands
           :assert-true
           :assert-false
           :assert-error)
  ;; Functions for managing tests
  (:export :define-test
           :list-tests
           :test-code
           :test-documentation
           :remove-tests
           :run-tests
           :run-koans
           :use-debugger)
  ;; Functions for managing tags
  (:export :list-tags
           :tagged-tests
           :remove-tags
           :run-tags)
  ;; Constants for blanks in koans
  (:export __
           ___
           ____
           +blanks+)
  ;; Functions for reporting test results
  (:export :test-names
           :failed-tests
           :incomplete-tests
           :error-tests
           :missing-tests
           :summarize-results
           :any-non-pass-p)
  ;; Utility predicates
  (:export :logically-equal :set-equal))

(in-package :lisp-unit)


;; blank constants allow the incomplete tests to compile without errors
(defconstant __ :blank-value)
(defconstant ___ :blank-value)
(defconstant ____ :blank-value)
(defconstant +blanks+ '(__ ___ ____))
(defconstant +blank-value+ 'BLANK-VALUE)


;;; Global counters

(defparameter *pass* 0
  "The number of passed assertions.")

(defparameter *fail* 0
  "The number of failed assertions.")

(defparameter *incomplete* 0
  "The number of incomplete assertions.")

(defparameter *koan-assert-list* nil
  "The record of a single koan.")

;;; Global options

(defparameter *proceed-after-failure* nil
  "set to nil for normal operation.  t will eval every koan")

(defparameter *print-summary* nil
  "Print a summary of the pass, fail, and error count if non-nil.")

(defparameter *print-failures* nil
  "Print failure messages if non-NIL.")

(defparameter *print-errors* nil
  "Print error messages if non-NIL.")

(defparameter *use-debugger* nil
  "If not NIL, enter the debugger when an error is encountered in an
assertion.")

(defun use-debugger-p (condition)
  "Debug or ignore errors."
  (cond
   ((eq :ask *use-debugger*)
    (y-or-n-p "~A -- debug?" condition))
   (*use-debugger*)))

(defun use-debugger (&optional (flag t))
  "Use the debugger when testing, or not."
  (setq *use-debugger* flag))

;;; Failure control strings

(defgeneric print-failure (type form expected actual extras)
  (:documentation
   "Report the details of the failure assertion."))

(defmethod print-failure :around (type form expected actual extras)
  "Failure header and footer output."
  (declare (ignore expected actual))
  (format t "~& | Failed Form: ~S" form)
  (call-next-method)
  (when extras
    (format t "~{~& | ~S => ~S~}~%" (funcall extras)))
  (format t "~& |~%")
  type)

(defmethod print-failure (type form expected actual extras)
  (declare (ignore type form extras))
  (format t "~& | Expected ~{~S~^; ~} " expected)
  (format t "~<~% | ~:;but saw ~{~S~^; ~}~>" actual))

(defmethod print-failure ((type (eql :error))
                          form expected actual extras)
  (declare (ignore form extras))
  (format t "~& | ~@[Should have signalled ~{~S~^; ~} but saw~]"
          expected)
  (format t " ~{~S~^; ~}" actual))

(defmethod print-failure ((type (eql :macro))
                          form expected actual extras)
  (declare (ignore form extras))
  (format t "~& | Should have expanded to ~{~S~^; ~} " expected)
  (format t "~<~%~:;but saw ~{~S~^; ~}~>" actual))

(defmethod print-failure ((type (eql :output))
                          form expected actual extras)
  (declare (ignore form extras))
  (format t "~& | Should have printed ~{~S~^; ~} " expected)
  (format t "~<~%~:;but saw ~{~S~^; ~}~>" actual))

(defun print-error (condition)
  "Print the error condition."
  (let ((*print-escape* nil))
    (format t "~& | Execution error:~% | ~W" condition)
    (format t "~& |~%")))

(defun print-summary (name pass fail incomplete &optional exerr)
  "Print a summary of the test results."
  (format t "~&~A: ~S assertions passed, ~S failed, ~S incomplete"
          name pass fail incomplete)
  (format t "~@[, ~S execution errors~].~2%" exerr))

;;; Global unit test database

(defparameter *test-db* (make-hash-table :test #'eq)
  "The unit test database is simply a hash table.")

(defun package-table (package &optional create)
  (cond
   ((gethash (find-package package) *test-db*))
   (create
    (setf (gethash package *test-db*) (make-hash-table)))
   (t (warn "No tests defined for package: ~S" package))))

;;; Global tags database

(defparameter *tag-db* (make-hash-table :test #'eq)
  "The tag database is simply a hash table.")

(defun package-tags (package &optional create)
  "Return the tags DB for the package."
  (cond
   ((gethash (find-package package) *tag-db*))
   (create
    (setf (gethash package *tag-db*) (make-hash-table)))
   (t (warn "No tags defined for package: ~S" package))))

(defclass unit-test ()
  ((doc
    :type string
    :initarg :doc
    :reader doc)
   (code
    :type list
    :initarg :code
    :reader code))
  (:default-initargs :doc "" :code ())
  (:documentation
   "Organize the unit test documentation and code."))

;;; NOTE: Shamelessly taken from PG's analyze-body
(defun parse-body (body &optional doc tag)
  "Separate the components of the body."
  (let ((item (first body)))
    (cond
     ((and (listp item) (eq :tag (first item)))
      (parse-body (rest body) doc (nconc (rest item) tag)))
     ((and (stringp item) (not doc) (rest body))
      (if tag
          (values doc tag (rest body))
          (parse-body (rest body) doc tag)))
     (t (values doc tag body)))))

(defmacro define-test (name &body body)
  "Store the test in the test database."
  (multiple-value-bind (doc tag code) (parse-body body)
    `(let ((doc (or ,doc (string ',name))))
       (setf
        ;; Unit test
        (gethash ',name (package-table *package* t))
        (make-instance 'unit-test :doc doc :code ',code))
       ;; Tags
       (loop for tag in ',tag do
             (pushnew
              ',name (gethash tag (package-tags *package* t))))
       ;; Return the name of the test
       ',name)))

;;; Manage tests

(defun list-tests (&optional (package *package*))
  "Return a list of the tests in package."
  (let ((table (package-table package)))
    (when table
      (loop for test-name being each hash-key in table
            collect test-name))))

(defun test-documentation (name &optional (package *package*))
  "Return the documentation for the test."
  (let ((unit-test (gethash name (package-table package))))
    (if (null unit-test)
        (warn "No code defined for test ~A in package ~S."
              name package)
        (doc unit-test))))

(defun test-code (name &optional (package *package*))
  "Returns the code stored for the test name."
  (let ((unit-test (gethash name (package-table package))))
    (if (null unit-test)
        (warn "No code defined for test ~A in package ~S."
              name package)
        (code unit-test))))

(defun remove-tests (names &optional (package *package*))
  "Remove individual tests or entire sets."
  (if (eq :all names)
      (if (null package)
          (clrhash *test-db*)
          (progn
            (remhash (find-package package) *test-db*)
            (remhash (find-package package) *tag-db*)))
      (let ((table (package-table package)))
        (unless (null table)
          ;; Remove tests
          (loop for name in names
                always (remhash name table)
                collect name into removed
                finally (return removed))
          ;; Remove tests from tags
          (loop with tags = (package-tags package)
                for tag being each hash-key in tags
                using (hash-value tagged-tests)
                do
                (setf
                 (gethash tag tags)
                 (set-difference tagged-tests names)))))))

;;; Manage tags

(defun %tests-from-all-tags (&optional (package *package*))
  "Return all of the tests that have been tagged."
  (loop for tests being each hash-value in (package-tags package)
        nconc (copy-list tests) into all-tests
        finally (return (delete-duplicates all-tests))))

(defun %tests-from-tags (tags &optional (package *package*))
  "Return the tests associated with the tags."
  (loop with table = (package-tags package)
        for tag in tags
        as tests = (gethash tag table)
        nconc (copy-list tests) into all-tests
        finally (return (delete-duplicates all-tests))))

(defun list-tags (&optional (package *package*))
  "Return a list of the tags in package."
  (let ((tags (package-tags package)))
    (when tags
      (loop for tag being each hash-key in tags collect tag))))

(defun tagged-tests (tags &optional (package *package*))
  "Run the tests associated with the specified tags in package."
  (if (eq :all tags)
      (%tests-from-all-tags package)
      (%tests-from-tags tags package)))

(defun remove-tags (tags &optional (package *package*))
  "Remove individual tags or entire sets."
  (if (eq :all tags)
      (if (null package)
          (clrhash *tag-db*)
          (remhash (find-package package) *tag-db*))
      (let ((table (package-tags package)))
        (unless (null table)
          (loop for tag in tags
                always (remhash tag table)
                collect tag into removed
                finally (return removed))))))

;;; Assert macros

(defmacro assert-eq (expected form &rest extras)
  "Assert whether expected and form are EQ."
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'eq))

(defmacro assert-eql (expected form &rest extras)
  "Assert whether expected and form are EQL."
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'eql))

(defmacro assert-equal (expected form &rest extras)
  "Assert whether expected and form are EQUAL."
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'equal))

(defmacro assert-equalp (expected form &rest extras)
  "Assert whether expected and form are EQUALP."
  `(expand-assert :equal ,form ,form ,expected ,extras :test #'equalp))

(defmacro true-or-false? (expected form &rest extras)
  "Assert whether expected and form are EQUAL."
  `(expand-assert :equal ,form (not (not ,form)) ,expected ,extras :test #'equal))

(defmacro assert-error (condition form &rest extras)
  "Assert whether form signals condition."
  `(expand-assert :error ,form (expand-error-form ,form)
                  ,condition ,extras))

(defmacro assert-expands (expansion form &rest extras)
  "Assert whether form expands to expansion."
  `(expand-assert :macro ,form
                  (expand-macro-form ,form nil)
                  ,expansion ,extras))

(defmacro assert-false (form &rest extras)
  "Assert whether the form is false."
  `(expand-assert :result ,form ,form nil ,extras))

(defmacro assert-equality (test expected form &rest extras)
  "Assert whether expected and form are equal according to test."
  `(expand-assert :equal ,form ,form ,expected ,extras :test ,test))

(defmacro assert-prints (output form &rest extras)
  "Assert whether printing the form generates the output."
  `(expand-assert :output ,form (expand-output-form ,form)
                  ,output ,extras))

(defmacro assert-true (form &rest extras)
  "Assert whether the form is true."
  `(expand-assert :result ,form ,form t ,extras))

(defmacro expand-assert (type form body expected extras &key (test '#'eql))
  "Expand the assertion to the internal format."
  `(internal-assert ,type ',form
                    (lambda () ,body)
                    (lambda () ,expected)
                    (expand-extras ,extras)
                    ,test))

(defmacro expand-error-form (form)
  "Wrap the error assertion in HANDLER-CASE."
  `(handler-case ,form
     (condition (error) error)))

(defmacro expand-output-form (form)
  "Capture the output of the form in a string."
  (let ((out (gensym)))
    `(let* ((,out (make-string-output-stream))
            (*standard-output*
             (make-broadcast-stream *standard-output* ,out)))
       ,form
       (get-output-stream-string ,out))))

(defmacro expand-macro-form (form env)
  "Expand the macro form once."
  `(macroexpand-1 ',form ,env))

(defmacro expand-extras (extras)
  "Expand extra forms."
  `(lambda ()
     (list ,@(mapcan (lambda (form) (list `',form form)) extras))))

;;; Test passed predicate.

(defgeneric test-passed-p (type expected actual test)
  (:documentation
   "Return the result of the test."))

(defmethod test-passed-p ((type (eql :error)) expected actual test)
  "Return the result of the error assertion."
  (declare (ignore test))
  (or
   (eql (car actual) (car expected))
   (typep (car actual) (car expected))))

(defmethod test-passed-p ((type (eql :equal)) expected actual test)
  "Return the result of the equality assertion."
  (and
   (<= (length expected) (length actual))
   (every test expected actual)))

(defmethod test-passed-p ((type (eql :macro)) expected actual test)
  "Return the result of the macro expansion."
  (declare (ignore test))
  (equal (car actual) (car expected)))

(defmethod test-passed-p ((type (eql :output)) expected actual test)
  "Return the result of the printed output."
  (declare (ignore test))
  (string=
   (string-trim '(#\newline #\return #\space) (car actual))
   (car expected)))

;;; (LOGICALLY-EQUAL x y) => true or false
;;;   Return true if x and y both false or both true
(defun logically-equal (x y)
  (eql (not x) (not y)))

(defmethod test-passed-p ((type (eql :result)) expected actual test)
  "Return the result of the assertion."
  (declare (ignore test))
  (logically-equal (car actual) (car expected)))

(defun form-contains-one-of-p (form symbol-list)
  ;; returns nil if form contains (recursively) no element of the symbol-list
  ;; otherwise it returns the first element of symbol-list that it finds
  ;; in form.
  (cond
    ((symbolp form) (find form symbol-list))
    ((listp form) (or (form-contains-one-of-p (car form) symbol-list)
                      (form-contains-one-of-p (cdr form) symbol-list)))
    (t nil)))

(defun internal-assert
       (type form code-thunk expected-thunk extras test)  
  "Perform the assertion and record the results."
  (let* ((expected (multiple-value-list (funcall expected-thunk)))
         (actual (multiple-value-list (funcall code-thunk)))
         (passed (test-passed-p type expected actual test))
         (incomplete (or (form-contains-one-of-p form +blanks+)
                         (form-contains-one-of-p expected '(:blank-value)))))

   (cond
      (incomplete (progn
                    (incf *incomplete*)
                    (push :incomplete *koan-assert-list*)))
      (passed (progn
                (incf *pass*)
                (push :pass *koan-assert-list*)))
      (t (progn
           (incf *fail*)
           (push :fail *koan-assert-list*))))
    ;; Report the assertion
    (when (and (not passed) *print-failures*)
      (print-failure type form expected actual extras))
    ;; Return the result
    passed))

;;; results

(defclass test-results ()
  ((test-names
    :type list
    :initarg :test-names
    :accessor test-names)
   (pass
    :type fixnum
    :initform 0
    :accessor pass)
   (fail
    :type fixnum
    :initform 0
    :accessor fail)
   (incomplete
    :type fixnum
    :initform 0
    :accessor incomplete)
   (exerr
    :type fixnum
    :initform 0
    :accessor exerr)
   (failed-tests
    :type list
    :initform ()
    :accessor failed-tests)
   (incomplete-tests
    :type list
    :initform ()
    :accessor incomplete-tests)
   (error-tests
    :type list
    :initform ()
    :accessor error-tests)
   (missing-tests
    :type list
    :initform ()
    :accessor missing-tests))
  (:default-initargs :test-names ())
  (:documentation
   "Store the results of the tests for further evaluation."))

(defmethod print-object ((object test-results) stream)
  "Print the summary counts with the object."
  (format stream "#<~A Total(~D) Passed(~D) Failed(~D) Incomplete(~D) Errors(~D)>~%"
          (class-name (class-of object))
          (+ (pass object) (fail object) (incomplete object))
          (pass object) (fail object) (incomplete object) (exerr object)))

(defun summarize-results (results)
  "Print a summary of all results."
  (let ((pass (pass results))
        (fail (fail results))
        (incomplete (incomplete results)))
    (format t "~&Unit Test Summary~%")
    (format t " | ~D assertions total~%" (+ pass fail incomplete))
    (format t " | ~D passed~%" pass)
    (format t " | ~D failed~%" fail)
    (format t " | ~D incomplete~%" incomplete)
    (format t " | ~D execution errors~%" (exerr results))
    (format t " | ~D missing tests~2%"
            (length (missing-tests results)))))

;;; Run the tests

(defun run-code (code)
  "Run the code to test the assertions."
  (funcall (coerce `(lambda () ,@code) 'function)))

(defun run-test-thunk (code)
  (let ((*pass* 0)
        (*fail* 0)
        (*incomplete* 0))
    (handler-bind
        ((error (lambda (condition)
                  (when *print-errors*
                    (print-error condition))
                  (if (use-debugger-p condition)
                      condition
                      (return-from run-test-thunk
                        (values *pass* *fail* *incomplete* condition))))))
      (run-code code))
    ;; Return the result count
    (values *pass* *fail* *incomplete* nil)))

(defun run-koan-thunk (code)
  (let ((*koan-assert-list* nil))
    (handler-bind
        ((error (lambda (condition)
                  (push :error *koan-assert-list*)
                  (when *print-errors*
                    (print-error condition))
                  (if (use-debugger-p condition)
                      condition
                      (return-from run-koan-thunk
                        (values *koan-assert-list* condition))))))
      (run-code code))
    ;; Return the result count
    (values *koan-assert-list* nil)))

(defun koan-result (code)
   "Run the code.  Return a list of assertion result elements.
    An assertion result element is one of :pass, :fail, :error, :incomplete"
   (run-koan-thunk code))

(defun record-result (test-name code results)
  "Run the test code and record the result."
  (multiple-value-bind (pass fail incomplete exerr)
      (run-test-thunk code)
    (push test-name (test-names results))
    ;; Count passed tests
    (when (plusp pass)
      (incf (pass results) pass))
    ;; Count failed tests and record name
    (when (plusp fail)
      (incf (fail results) fail)
      (push test-name (failed-tests results)))
    ;; Count incomplete tests and record name
    (when (plusp incomplete)
      (incf (incomplete results) incomplete)
      (push test-name (incomplete-tests results)))
    ;; Count errors and record name
    (when exerr
      (incf (exerr results))
      (push test-name (error-tests results)))
    ;; Print a summary of the results
    (when (or *print-summary* *print-failures* *print-errors*)
      (print-summary
       test-name pass fail incomplete (when exerr 1)))))

(defun %run-all-thunks (&optional (package *package*))
  "Run all of the test thunks in the package."
  (loop
   with results = (make-instance 'test-results)
   for test-name being each hash-key in (package-table package)
   using (hash-value unit-test)
   if unit-test do
   (record-result test-name (code unit-test) results)
   else do
   (push test-name (missing-tests results))
   ;; Summarize and return the test results
   finally
   (summarize-results results)
   (return results)))

(defun %run-thunks (test-names &optional (package *package*))
  "Run the list of test thunks in the package."
  (loop
   with table = (package-table package)
   and results = (make-instance 'test-results)
   for test-name in test-names
   as unit-test = (gethash test-name table)
   if unit-test do
   (record-result test-name (code unit-test) results)
   else do
   (push test-name (missing-tests results))
   finally
   (summarize-results results)
   (return results)))

(defun run-koans (package)
  "Run the list of test thunks in the package. Stopping
   at a failure or incomplete, with more helpful messaging"
  (loop
    with koan-results = nil
    for test-name being each hash-key in (package-table package)
    using (hash-value unit-test)
    if unit-test do
      (push (list test-name (koan-result (code unit-test))) koan-results)
    else do
      (push (list test-name :missing) koan-results)
    until (and (not *proceed-after-failure*) (any-non-pass-p koan-results))
    finally (return koan-results)))

(defun any-non-pass-p (koan-results)
  (dolist (one-koan koan-results)
    (dolist (assert-result (second one-koan))
      (if (not (equal :pass assert-result))
          (return-from any-non-pass-p t))))
  nil)

(defun run-tests (test-names &optional (package *package*))
  "Run the specified tests in package."
  (if (eq :all test-names)
      (%run-all-thunks package)
      (%run-thunks test-names package)))

(defun run-tags (tags &optional (package *package*))
  "Run the tests associated with the specified tags in package."
  (%run-thunks (tagged-tests tags package) package))

;;; (SET-EQUAL l1 l2 :test) => true or false
;;;   Return true if every element of l1 is an element of l2
;;;   and vice versa.
(defun set-equal (l1 l2 &key (test #'equal))
  (and (listp l1)
       (listp l2)
       (subsetp l1 l2 :test test)
       (subsetp l2 l1 :test test)))

(pushnew :lisp-unit common-lisp:*features*)
