(defpackage :xmas.runloop (:use :cl :alexandria) (:export #:make-runloop #:kill-runloop #:enqueue-runloop-event))
(in-package :xmas.runloop)

(defstruct (runloop (:constructor %make-runloop))
  thread
  queue)

(defun make-runloop (&key (name "runloop") (step (/ 1.0 60.0))
                       function event-handler)
  (let ((queue (queues:make-queue :simple-cqueue)))
    (%make-runloop
     :thread
     (bt:make-thread
      (lambda ()
        (let ((dt step) start end wait (first-step t)
              (unit internal-time-units-per-second))
          (declare (ignorable dt))
          (loop do
               (loop for event = (queues:qpop queue)
                  while event
                  do (funcall event-handler event))
               (unless first-step
                 (setf dt (/ (- (get-internal-real-time) start)
                             unit)))
               (setf
                first-step nil
                start (get-internal-real-time))
               (funcall function
                        ;; not sure what's going on yet, but using dt
                        ;; appears to give stuttering...
                        ;; just hardcoding the step appears to give smoother
                        ;; results...
                        ;; dt
                        step
                        )
               (setf end (get-internal-real-time))
               (let ((elapsed (/ (- end start) unit)))
                 (setf wait (- step (mod elapsed step))))
               (sleep wait))))
      :name name)
     :queue queue)))

(defun enqueue-runloop-event (runloop event)
  (queues:qpush (runloop-queue runloop) event))

(defun kill-runloop (runloop)
  (bt:destroy-thread (runloop-thread runloop)))
