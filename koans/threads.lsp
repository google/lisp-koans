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

;; NOTE: This koan group uses quicklisp to load packages that are
;; not part of the Common Lisp specification.
;; If you are using quicklisp please feel free to enable this group
;; by following the instructions in the README.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making threads with bordeaux-threads:make-thread  ;;
;; Joining threads with bordeaux-threads:join-thread ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bordeaux-threads takes a -function- as a parameter.
;; This function will be executed in a separate thread.

;; Since the execution order of separate threads is not guaranteed,
;; we must -join- the threads in order to make our assertions.

;; (load "~/.quicklisp/setup.lisp")
(ql:quickload :bordeaux-threads)
(ql:quickload :bt-semaphore)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending arguments to the thread function: ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun returns-hello-name (name)
  (format nil "Hello, ~a" name))

(defun double-wrap-list (x y z)
  (list (list x y z)))

;; Create a thread which will print out "Hello -Name-" using
;; the named write-hello-name function.  Arguments and functions
;; are handed to threads in a lambda.

(define-test test-sending-arguments-to-thread
    (assert-equal "Hello, Buster"
                  (bordeaux-threads:join-thread
                   (bordeaux-threads:make-thread
                    #'(lambda ()
                        (returns-hello-name "Buster")))))
  (assert-equal ____
                (bordeaux-threads:join-thread
                 (bordeaux-threads:make-thread
                  #'(lambda ()
                      (double-wrap-list 3 4 5))))))


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
  (let ((thread-1 (bordeaux-threads:make-thread #'(lambda () (accum-after-time 0.3 1))))
        (thread-2 (bordeaux-threads:make-thread #'(lambda () (accum-after-time 0.2 2))))
        (thread-3 (bordeaux-threads:make-thread #'(lambda () (accum-after-time 0.1 4)))))
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

(defun main-thread-p (thread)
  (string-equal (bordeaux-threads:thread-name thread)
                "Main Thread"))

(defun kill-thread-if-not-main (thread)
" kills a given thread, unless the thread is the main thread.
 returns nil if thread is main.
 returns a 'terminated~' string otherwise"
  (unless (string-equal (bordeaux-threads:thread-name thread)
                        "Main Thread")
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
(defvar *gs-mutex* (bordeaux-threads:make-lock "g's lock"))

(defun protected-increments-g (&optional (n 0.1))
  "Surround all references to *g* within the with-mutex form."
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

;;;;;;;;;;;;;;;;
;; Semaphores ;;
;;;;;;;;;;;;;;;;

;; Incrementing a semaphore is an atomic operation.
(defvar *g-semaphore* (bordeaux-threads:make-semaphore :name "g" :count 0))

(defun semaphore-increments-g ()
  (bordeaux-threads:signal-semaphore *g-semaphore*))

(define-test test-increment-semaphore
    (assert-equal 0 (bt-semaphore:semaphore-count *g-semaphore*))
  (bordeaux-threads:join-thread (bordeaux-threads:make-thread 'semaphore-increments-g :name "S incrementor 1"))
  (bordeaux-threads:join-thread (bordeaux-threads:make-thread 'semaphore-increments-g :name "S incrementor 2"))
  (bordeaux-threads:join-thread (bordeaux-threads:make-thread 'semaphore-increments-g :name "S incrementor 3"))
  (assert-equal ___ (bt-semaphore:semaphore-count *g-semaphore*)))


;; Semaphores can be used to manage resource allocation, and to trigger
;; threads to run when the semaphore value is above zero.

(defvar *apples* (bt-semaphore:make-semaphore :name "how many apples" :count 0))
(defvar *orchard-log* (make-array 10))
(defvar *next-log-entry* 0)
(defvar *orchard-log-mutex* (bordeaux-threads:make-lock "orchard log mutex"))

(defun add-to-log (item)
  (bordeaux-threads:with-lock-held (*orchard-log-mutex*)
    (setf (aref *orchard-log* *next-log-entry*) item)
    (incf *next-log-entry*)))

(defun apple-eater ()
  (bt-semaphore:wait-on-semaphore *apples*)
  (add-to-log "apple eaten."))

(defun apple-grower ()
  (sleep 0.1)
  (add-to-log "apple grown.")
  (bt-semaphore:signal-semaphore *apples*))

(defun num-apples ()
  (bt-semaphore:semaphore-count *apples*))

(define-test test-orchard-simulation
    (assert-equal (num-apples) ___)
  (let ((eater-thread (bordeaux-threads:make-thread 'apple-eater :name "apple eater thread")))
    (let ((grower-thread (bordeaux-threads:make-thread 'apple-grower :name "apple grower thread")))
      (bordeaux-threads:join-thread eater-thread)))
  (assert-equal (aref *orchard-log* 0) ____)
  (assert-equal (aref *orchard-log* 1) ____))
