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

(in-package :cl-user)

;;; Though Clozure / CCL runs lisp-koans on the command line using
;;; "ccl -l contemplate.lisp", the following lines are needed to
;;; meditate on the koans within the CCL IDE.
;;; (The :hemlock is used to distiguish between ccl commandline and the IDE)
#+(and :ccl :hemlock)
(setf *default-pathname-defaults* (directory-namestring *load-pathname*))

(load "test-framework.lisp")
(load "lisp-koans.lisp")

#+quicklisp (ql:quickload :bordeaux-threads)

(lisp-koans.core:main)
