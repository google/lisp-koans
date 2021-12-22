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

;;; This lesson group uses Quicklisp to load Bordeaux Threads, a portability
;;; library for working with threads. This is because threads are not a part of
;;; the Common Lisp standard and implementations do them differently.
;;; If you are using Quicklisp, please feel free to enable this lesson by
;;; following the instructions in the README.

;;; TODO: wait for Bordeaux Threads to implement a portable SEMAPHORE-COUNT
;;; and use it in the semaphore koans.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test thread-return-value
  ;; When a thread object is constructed, it accepts a function to execute.
  (let* ((thread (bt:make-thread (lambda () (+ 2 2))))
         ;; When the thread's function finishes, its return value becomes the
         ;; return value of BT:JOIN-THREAD.
         (value (bt:join-thread thread)))
    (assert-equal ____ value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *variable*)

(define-test thread-global-bindings
  ;; The global value of a variable is shared between all threads.
  (setf *variable* 42)
  (let ((thread (bt:make-thread (lambda ()
                                  (when (= *variable* 42)
                                    (setf *variable* 24)
                                    t)))))
    (assert-true (bt:join-thread thread))
    (assert-equal ____ *variable*)))

(define-test thread-local-bindings
  ;; Newly established local bindings of a variable are visible only in the
  ;; thread that established these bindings.
  (setf *variable* 42)
  (let ((thread (bt:make-thread (lambda ()
                                  (let ((*variable* 42))
                                    (setf *variable* 24))))))
    (bt:join-thread thread)
    (assert-equal ____ *variable*)))

(define-test thread-initial-bindings
  ;; Initial dynamic bindings may be passed to the new thread.
  (setf *variable* 42)
  (let ((thread (bt:make-thread (lambda () (setf *variable* 24))
                                :initial-bindings '((*variable* . 42)))))
    (bt:join-thread thread)
    (assert-equal ____ *variable*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test thread-name
  ;; Threads can have names.
  (let ((thread (bt:make-thread #'+ :name "Summing thread")))
    (assert-equal ____ (bt:thread-name thread))
    (assert-equal ____ (bt:join-thread thread))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test thread-function-arguments
  ;; Passing arguments to thread functions requires closing over them.
  (let* ((x 240)
         (y 18)
         (thread (bt:make-thread (lambda () (* x y)))))
    (assert-equal ____ (bt:join-thread thread))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test destroy-thread
  ;; Looping and renegade threads can usually be killed via BT:DESTROY-THREAD.
  ;; It is the last measure, since doing so might leave the Lisp system in an
  ;; unpredictable state if the thread was doing something complex.
  (let ((thread (bt:make-thread (lambda () (loop (sleep 10))))))
    (true-or-false? ____ (bt:thread-alive-p thread))
    (bt:destroy-thread thread)
    (sleep 1)
    (true-or-false? ____ (bt:thread-alive-p thread))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *another-variable*)

;; Preventing concurrent access to some data can be achieved via a lock in
;; order to avoid race conditions.

(defvar *lock* (bt:make-lock))

(define-test lock
  (setf *another-variable* 0)
  (flet ((increaser () (bt:with-lock-held (*lock*) (incf *another-variable*))))
    (loop repeat 100
          collect (bt:make-thread #'increaser) into threads
          finally (loop until (notany #'bt:thread-alive-p threads))
                  (assert-equal ____ *another-variable*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We can further orchestrate threads by using semaphores.

(defvar *semaphore* (bt:make-semaphore))

(defun signal-our-semaphore ()
  (bt:signal-semaphore semaphore))

(defun wait-on-our-semaphore ()
  (bt:wait-on-semaphore semaphore :timeout 100))

(define-test semaphore
  (assert-equal 1 (bt:join-thread (bt:make-thread #'signal-our-semaphore)))
  (assert-equal ____ (bt:join-thread (bt:make-thread #'signal-our-semaphore)))
  (assert-equal ____ (bt:join-thread (bt:make-thread #'signal-our-semaphore)))
  (assert-equal 2 (bt:join-thread (bt:make-thread #'wait-on-our-semaphore)))
  (assert-equal ____ (bt:join-thread (bt:make-thread #'wait-on-our-semaphore)))
  (assert-equal ____ (bt:join-thread (bt:make-thread #'wait-on-our-semaphore)))
  (assert-equal ____ (bt:join-thread (bt:make-thread #'wait-on-our-semaphore))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Semaphores can be used to manage resource allocation and to trigger some
;; threads to run when the semaphore value is above zero.

(defvar *foobar-semaphore* (bt:make-semaphore))

(defvar *foobar-list*)

(defun bar-pusher ()
  (dotimes (i 10)
    (sleep 0.01)
    (push i (nth i *foobar-list*))
    (push :bar (nth i *foobar-list*))
    ;; We push :BAR before :FOO, so the final list looks like (:FOO :BAR).
    (bt:signal-semaphore *foobar-semaphore*)))

(defun foo-pusher ()
  (dotimes (i 10)
    (bt:wait-on-semaphore *foobar-semaphore*)
    (push :foo (nth i *foobar-list*))))

(define-test list-of-foobars
  (setf *foobar-list* (make-list 10))
  (let ((bar-pusher (bt:make-thread #'bar-pusher))
        (foo-pusher (bt:make-thread #'foo-pusher)))
    (bt:join-thread foo-pusher))
  (assert-equal ____ (nth 0 *foobar-list*))
  (assert-equal ____ (nth 1 *foobar-list*))
  (assert-equal ____ (nth 5 *foobar-list*)))
