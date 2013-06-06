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

;; NOTE: This koan group uses language features specific to bordeaux-threads,
;; that are not part of the Common Lisp specification. Feel free to skip this
;; group by removing it from '.koans'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making threads with bordeaux-threads:make-thread  ;;
;; Joining threads with bordeaux-threads:join-thread ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-thread takes a -function- as a parameter.
;; This function will be executed in a separate thread.

;; Since the execution order of separate threads is not guaranteed,
;; we must -join- the threads in order to make our assertions.

(defvar *greeting* "no greeting")

(defun sets-socal-greeting ()
  (setf *greeting* "Sup, dudes"))

(define-test test-hello-world-thread
    "Create a thread which returns 'hello world', then ends.
    using a lambda as the supplied function to execute."
  (assert-equal *greeting* "no greeting")
  (let ((greeting-thread
         (bordeaux-threads:make-thread
          (lambda ()
            (setf *greeting* "hello world")))))
    (bordeaux-threads:join-thread greeting-thread)
    (assert-equal *greeting* "hello world")
    (setf greeting-thread (bordeaux-threads:make-thread #'sets-socal-greeting))
    (bordeaux-threads:join-thread greeting-thread)
    (assert-equal *greeting* ____)))


(define-test test-join-thread-return-value
    "the return value of the thread is passed in bordeaux-threads:join-thread"
  (let ((my-thread (bordeaux-threads:make-thread
                    (lambda () (* 11 99)))))
    (assert-equal ____ (bordeaux-threads:join-thread my-thread))))


(define-test test-threads-can-have-names
    "Threads can have names.  Names can be useful in diagnosing problems
     or reporting."
  (let ((empty-plus-thread
         (bordeaux-threads:make-thread #'+
                                :name "what is the sum of no things adding?")))
    (assert-equal (bordeaux-threads:thread-name empty-plus-thread)
                  ____)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simulate sending arguments to the thread function: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun returns-hello-name (name)
  (format nil "Hello, ~a" name))

(defun double-wrap-list (x y z)
  (list (list x y z)))

;; Create a thread which will print out "Hello -Name-" using the named
;; write-hello-name function.  We cannot pass a function taking argument
;; directly to make-thread, so we must pass a lambda that will funcall the
;; function with the argument values.

(define-test test-sending-arguments-to-thread
  (assert-equal "Hello, Buster" 
                (bordeaux-threads:join-thread
                 (bordeaux-threads:make-thread 
                  (lambda () (funcall #'returns-hello-name "Buster")))))
  (assert-equal ____
                (bordeaux-threads:join-thread
                 (bordeaux-threads:make-thread 
                  (lambda () (funcall #'double-wrap-list 3 4 5))))))

;; ----

(defvar *accum* 0)

(defun accum-after-time (time arg1)
    "sleeps for time seconds and then adds arg1 to *accum*"
  (sleep time)
  (incf *accum* arg1))

(defvar *before-time-millisec* 0)
(defvar *after-time-millisec* 0)

;; cheap and dirty time measuring function
(defun duration-ms ()
  (- *after-time-millisec* *before-time-millisec*))

(define-test test-run-in-series
    "get internal real time returns a time stamp in milliseconds"
  (setf *accum* 0)
  (setf *before-time-millisec* (get-internal-real-time))
  (accum-after-time 0.3 1)
  (accum-after-time 0.2 2)
  (accum-after-time 0.1 4)
  (setf *after-time-millisec* (get-internal-real-time))
  (true-or-false? ___ (> (duration-ms) 500))
  (true-or-false? ___ (< (duration-ms) 700))
  (assert-equal *accum* ___))

(define-test test-run-in-parallel
    "same program as above, executed in threads.  Sleeps are simultaneous"
  (setf *accum* 0)
  (setf *before-time-millisec* (get-internal-real-time))
  (let ((thread-1 (bordeaux-threads:make-thread 
                   (lambda () (funcall 'accum-after-time 0.3 1))))
        (thread-2 (bordeaux-threads:make-thread
                   (lambda () (funcall 'accum-after-time 0.2 2))))
        (thread-3 (bordeaux-threads:make-thread
                   (lambda () (funcall 'accum-after-time 0.1 4)))))
    (bordeaux-threads:join-thread thread-1)
    (bordeaux-threads:join-thread thread-2)
    (bordeaux-threads:join-thread thread-3))
  (setf *after-time-millisec* (get-internal-real-time))
  (true-or-false? ___ (> (duration-ms) 200))
  (true-or-false? ___  (< (duration-ms) 400))
  (assert-equal *accum* ___))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; killing renegade threads            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun spawn-looping-thread (name)
  "create a never-ending looping thread with a given name"
  (bordeaux-threads:make-thread (lambda () (loop)) :name name))

(defvar *top-thread* (bordeaux-threads:current-thread))
(defun main-thread-p (thread) (eq thread *top-thread*))

(defun kill-thread-if-not-main (thread)
" kills a given thread, unless the thread is the main thread.
 returns nil if thread is main.
 returns a 'terminated~' string otherwise"
  (unless (main-thread-p thread)
    (bordeaux-threads:destroy-thread thread)
    (concatenate 'string "terminated " (bordeaux-threads:thread-name thread))))

(defun kill-spawned-threads ()
  "kill all lisp threads except the main thread."
  (map 'list 'kill-thread-if-not-main (bordeaux-threads:all-threads)))

(defun spawn-three-loopers ()
  "Spawn three run-aways."
  (progn
    (spawn-looping-thread "looper one")
    (spawn-looping-thread "looper two")
    (spawn-looping-thread "looper three")))

(define-test test-counting-and-killing-threads
    "all-threads makes a list of all running threads in this lisp.  The sleep
     calls are necessary, as killed threads are not instantly removed from the
     list of all running threads."
  (assert-equal ___ (length (bordeaux-threads:all-threads)))
  (kill-thread-if-not-main (spawn-looping-thread "NEVER CATCH ME~!  NYA NYA!"))
  (sleep 0.01)
  (assert-equal ___ (length (bordeaux-threads:all-threads)))
  (spawn-three-loopers)
  (assert-equal ___ (length (bordeaux-threads:all-threads)))
  (kill-spawned-threads)
  (sleep 0.01)
  (assert-equal ___ (length (bordeaux-threads:all-threads))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bindings are not inherited across threads ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *v* 0)

(defun returns-v ()
  *v*)

(define-test test-threads-dont-get-bindings
    "bindings are not inherited across threads"
  (let ((thread-ret-val (bordeaux-threads:join-thread
                         (bordeaux-threads:make-thread 'returns-v))))
    (assert-equal thread-ret-val ____))
  (let ((*v* "LEXICAL BOUND VALUE"))
    (assert-equal *v* ____)
    (let ((thread-ret-val (bordeaux-threads:join-thread
                           (bordeaux-threads:make-thread 'returns-v))))
      (assert-equal thread-ret-val ____))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global state (special vars) are ;;
;; shared across threads           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *g* 0)

(defun waits-and-increments-g (&optional (n 0.2))
  "sets *g* to 1 + the value of *g* n seconds ago"
  (let ((my-remembered-g *g*))
    (sleep n)
    (setq *g* (+ 1 my-remembered-g))))

(define-test test-serial-wait-and-increment
 "incrementing *g* three times and expecting
  the final value to be three works."
  (setf *g* 0)
  (waits-and-increments-g)
  (waits-and-increments-g)
  (waits-and-increments-g)
  (assert-equal *g* ___))


(define-test test-parallel-wait-and-increment
    (setf *g* 0)
  (let ((thread-1 (bordeaux-threads:make-thread 'waits-and-increments-g))
        (thread-2 (bordeaux-threads:make-thread 'waits-and-increments-g))
        (thread-3 (bordeaux-threads:make-thread 'waits-and-increments-g)))
    (bordeaux-threads:join-thread thread-1)
    (bordeaux-threads:join-thread thread-2)
    (bordeaux-threads:join-thread thread-3)
    (assert-equal *g* ___)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state can be protected ;;
;; with a mutex.                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *g* 0)
(defvar *gs-mutex* (bordeaux-threads:make-lock :name "g's lock"))

(defun protected-increments-g (&optional (n 0.1))
  "Surround all references to *g* within the with-lock-held form."
  (bordeaux-threads:with-lock-held (*gs-mutex*)
    (let ((my-remembered-g *g*))
      (sleep n)
      (setq *g* (+ 1 my-remembered-g)))))

(define-test test-parallel-wait-and-increment-with-mutex
    (setf *g* 0)
  (let ((thread-1 (bordeaux-threads:make-thread 'protected-increments-g))
        (thread-2 (bordeaux-threads:make-thread 'protected-increments-g))
        (thread-3 (bordeaux-threads:make-thread 'protected-increments-g)))
    (bordeaux-threads:join-thread thread-1)
    (bordeaux-threads:join-thread thread-2)
    (bordeaux-threads:join-thread thread-3)
    (assert-equal *g* ___)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Condition variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *g* 0)
(defvar *g-condvar* (bordeaux-threads:make-condition-variable :name "g's condition variable"))

(defun condvar-increments-g ()
  (bordeaux-threads:with-lock-held (*gs-mutex*)
    (incf *g*)
    (bordeaux-threads:condition-notify *g-condvar*)))

(define-test test-increment-with-condvar
  (bordeaux-threads:with-lock-held (*gs-mutex*)
    (assert-equal 0 *g*))
  
  (bordeaux-threads:join-thread 
   (bordeaux-threads:make-thread 'condvar-increments-g :name "S incrementor 1"))
  (bordeaux-threads:join-thread 
   (bordeaux-threads:make-thread 'condvar-increments-g :name "S incrementor 2"))
  (bordeaux-threads:join-thread 
   (bordeaux-threads:make-thread 'condvar-increments-g :name "S incrementor 3"))
  
  (bordeaux-threads:with-lock-held (*gs-mutex*)
    (bordeaux-threads:condition-wait *g-condvar* *gs-mutex*)
    (assert-equal ___ *g*)))


;; Condition variables can be used to manage resource allocation, and to
;; trigger threads to run when a varialbe's value is above zero.

(defvar *apples* 0)
(defvar *apples-condvar* (bordeaux-threads:make-condition-variable 
                          :name "apples condition variable"))
(defvar *orchard-log* (make-array 10))
(defvar *next-log-idx* 0)
(defvar *orchard-log-mutex* (bordeaux-threads:make-lock :name "orchard log mutex"))

(defun add-to-log (item)
  (bordeaux-threads:with-lock-held (*orchard-log-mutex*)
    (setf (aref *orchard-log* *next-log-idx*) item)
    (incf *next-log-idx*)))

(defun apple-eater ()
  (bordeaux-threads:with-lock-held (*orchard-log-mutex*)
    (bordeaux-threads:condition-wait *apples-condvar* *orchard-log-mutex*)
    (add-to-log "apple eaten.")))

(defun apple-grower ()
  (sleep 0.1)
  (add-to-log "apple grown.")
  (bordeaux-threads:with-lock-held (*orchard-log-mutex*)
    (incf *apples*)
    (bordeaux-threads:condition-notify *apples-condvar*)))

(defun num-apples ()
  (bordeaux-threads:with-lock-held (*orchard-log-mutex*)
    *apples*))

(define-test test-orchard-simulation
    (assert-equal (num-apples) ___)
  (let ((eater-thread (bordeaux-threads:make-thread 'apple-eater :name "apple eater thread")))
    (let ((grower-thread (bordeaux-threads:make-thread 'apple-grower :name "apple grower thread")))
      (bordeaux-threads:join-thread eater-thread)))
  (assert-equal (aref *orchard-log* 0) ____)
  (assert-equal (aref *orchard-log* 1) ____))




