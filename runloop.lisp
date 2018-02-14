(defpackage :xmas.runloop (:use :cl :alexandria) (:export #:make-runloop #:kill-runloop #:enqueue-runloop-event))
(in-package :xmas.runloop)

(defstruct (runloop (:constructor %make-runloop))
  thread
  queue)

(defun make-runloop (&key (name "runloop") (step (/ 1.0 60.0))
                       function event-handler bindings)
  (let ((queue (queues:make-queue :simple-cqueue)))
    (%make-runloop
     :thread
     (bt:make-thread
      (lambda ()
        (progv (mapcar 'car bindings) (mapcar 'cdr bindings)
          (let ((dt step) start end wait (first-step t)
                (unit internal-time-units-per-second))
            (declare (ignorable dt))
            (loop do
                 (loop for event = (queues:qpop queue)
                    while event
                    do
                      (restart-case
                          (funcall event-handler event)
                        (handle-next-event ()
                          :report "Handle the next event."
                          nil)))
                 (unless first-step
                   (setf dt (/ (- (get-internal-real-time) start)
                               unit)))
                 (setf
                  first-step nil
                  start (get-internal-real-time))
                 (restart-case
                     (funcall function
                              ;; not sure what's going on yet, but using dt
                              ;; appears to give stuttering...
                              ;; just hardcoding the step appears to give smoother
                              ;; results...
                              dt
                              ;; step
                              )
                   (skip-this-frame ()
                     :report "Skip this frame."
                     nil))
                 (setf end (get-internal-real-time))
                 (let ((elapsed (/ (- end start) unit)))
                   (setf wait (- step (mod elapsed step))))
                 (sleep wait)))))
      :name name)
     :queue queue)))

(defun enqueue-runloop-event (runloop event)
  (queues:qpush (runloop-queue runloop) event))

(defun kill-runloop (runloop)
  (bt:destroy-thread (runloop-thread runloop)))
