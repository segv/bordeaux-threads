;;;; -*- indent-tabs-mode: nil -*-

#|
Copyright 2006, 2007 Greg Pfeil

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:bordeaux-threads)

;;; documentation on the OpenMCL Threads interface can be found at
;;; http://openmcl.clozure.com/Doc/Programming-with-Threads.html

(deftype thread ()
  'ccl:process)

;;; Thread Creation

(defun %make-thread (function name)
  (ccl:process-run-function name function))

(defun current-thread ()
  ccl:*current-process*)

(defun threadp (object)
  (typep object 'ccl:process))

(defun thread-name (thread)
  (ccl:process-name thread))

;;; Resource contention: locks and recursive locks

(defun make-lock (&optional name)
  (ccl:make-lock (or name "Anonymous lock")))

(defun acquire-lock (lock &optional (wait-p t))
  (if wait-p
      (ccl:grab-lock lock)
      (ccl:try-lock lock)))

(defun release-lock (lock)
  (ccl:release-lock lock))

(defmacro with-lock-held ((place) &body body)
  `(ccl:with-lock-grabbed (,place)
     ,@body))

(defun make-recursive-lock (&optional name)
  (ccl:make-lock (or name "Anonymous recursive lock")))

(defun acquire-recursive-lock (lock)
  (ccl:grab-lock lock))

(defun release-recursive-lock (lock)
  (ccl:release-lock lock))

(defmacro with-recursive-lock-held ((place) &body body)
  `(ccl:with-lock-grabbed (,place)
     ,@body))

;;; Resource contention: condition variables

(defstruct (condition-variable (:constructor %make-condition-variable))
  name
  semaphore
  count)

(defun make-condition-variable (&key name)
  (%make-condition-variable :name name
                            :count 0
                            :semaphore (ccl:make-semaphore)))

(defun condition-wait (condition-variable lock &key timeout)
  (incf (condition-variable-count condition-variable))
  (release-lock lock)
  (unwind-protect
       (if timeout
           (ccl:timed-wait-on-semaphore (condition-variable-semaphore condition-variable) timeout)
           (ccl:wait-on-semaphore (condition-variable-semaphore condition-variable)))
    (acquire-lock lock t)
    (decf (condition-variable-count condition-variable))))

(defun condition-notify (condition-variable)
  (condition-broadcast condition-variable 1))

(defun condition-broadcast (condition-variable &optional (n nil n-p))
  (dotimes (i (max 0
                   (if n-p
                       (min n (condition-variable-count condition-variable))
                       (condition-variable-count condition-variable))))
    (ccl:signal-semaphore (condition-variable-semaphore condition-variable))))

(defun thread-yield ()
  (ccl:process-allow-schedule))

;;; Introspection/debugging

(defun all-threads ()
  (ccl:all-processes))

(defun interrupt-thread (thread function &rest args)
  (declare (dynamic-extent args))
  (apply #'ccl:process-interrupt thread function args))

(defun destroy-thread (thread)
  (signal-error-if-current-thread thread)
  (ccl:process-kill thread))

(defun thread-alive-p (thread)
  (not (ccl:process-exhausted-p thread)))

(defun join-thread (thread)
  (ccl:join-process thread))

(mark-supported)
