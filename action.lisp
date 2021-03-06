(defpackage :xmas.action
  (:use :cl :alexandria)
  (:export
   #:start-with-target
   #:step-action
   #:stop
   #:stopped-p
   #:rotate-by
   #:repeat-forever
   #:run-sequence
   #:call-next-method
   #:delay
   #:ease-in-sine
   #:ease-out-sine
   #:ease-in-out-sine
   #:ease-in-quad
   #:ease-out-quad
   #:ease-in-out-quad
   #:ease-in-cubic
   #:ease-in-quart
   #:ease-in-quint
   #:ease-in-quadratic
   #:callfunc
   #:find-easing-function
   #:move-by
   #:fade-in
   #:fade-out
   #:sprite-animation-action
   #:move-by-x
   #:move-by-y
   #:tint-to
   #:scale-x-to
   #:scale-y-to
   #:move-to
   #:lerp-slot-to
   #:hue-cycle
   #:blink
   #:ease
   #:hue-cycle-with-offset))
(in-package :xmas.action)

(defmacro with-struct ((prefix &rest slots) var &body body)
  (once-only (var)
    `(symbol-macrolet
         ,(loop for slot in slots collect
               (list slot (list (symbolicate prefix slot) var)))
       ,@body)))

(defstruct action
  target
  running)

(defmethod start-with-target ((self action) target)
  (setf (action-target self) target
        (action-running self) t))

(defmethod reset ((self action))
  (setf (action-running self) t))

(defmethod step-action ((self action) dt)
  (declare (ignorable self dt))
  (error "subclasses must override action:step-action"))

(defmethod stop ((self action))
  (setf (action-running self) nil))

(defmethod stopped-p ((self action))
  (not (action-running self)))

(defstruct (finite-time-action (:include action))
  (duration 0.0)
  (elapsed 0.0))

(defmethod update ((self finite-time-action) time)
  (declare (ignorable time))
  (error "finite-time-action subclasses must override action:update"))

(defmethod step-action ((self finite-time-action) dt)
  (with-struct (finite-time-action- elapsed duration) self
    (incf elapsed dt)
    (update self (clamp (/ elapsed duration) 0.0 1.0))
    (when (>= elapsed duration)
      (stop self))))

(defmethod reset ((self finite-time-action))
  (call-next-method)
  (setf (finite-time-action-elapsed self) 0.0))

;; ======================================================================
;; repeat forever and sequence

(defstruct (repeat-forever (:include action))
  action)

(defmethod start-with-target ((self repeat-forever) node)
  (call-next-method)
  (start-with-target (repeat-forever-action self) node))

(defmethod step-action ((self repeat-forever) dt)
  (with-struct (repeat-forever- action) self
    (when (stopped-p action)
      (reset action))
    (step-action action dt)))

(defmethod reset ((self repeat-forever))
  (call-next-method)
  (reset (repeat-forever-action self)))

(defun repeat-forever (action)
  (make-repeat-forever :action action))

(defstruct (run-sequence (:include finite-time-action))
  item0 item1 (prev -1)  split)

(defmethod reset ((self run-sequence))
  (call-next-method)
  (with-struct (run-sequence- item0 item1 prev) self
    (setf prev -1)
    (reset item0)
    (reset item1)))

(defmethod stop ((self run-sequence))
  (call-next-method)
  (with-struct (run-sequence- item0 item1 prev) self
    (when (not (= prev -1))
      (if (= prev 0)
          (stop item0)
          (stop item1)))))

(defmethod start-with-target ((self run-sequence) node)
  (declare (ignorable node))
  (call-next-method)
  (with-struct (run-sequence- item0 duration split prev) self
    (flet ((get-duration (item)
           (let ((d (finite-time-action-duration item)))
             (if (= d 0.0) single-float-epsilon d))))
      (setf
       split (/ (get-duration item0) duration)
       prev -1))
    (when (= split 0.0) (setf split single-float-epsilon))
    (when (= split 1.0) (setf split (- 1.0 single-float-epsilon))))) 

(defmethod update ((self run-sequence) time)
  (with-struct (run-sequence- item0 item1 target split prev) self
    (let (found new-time action)
      (if (< time split)
          (setf found 0
                new-time (if (= split 0.0) time (/ time split)))
          (setf found 1
                new-time (/ (- time split) (- 1.0 split))))
      (cond
        ((= found 1)
         (cond
           ((= prev -1)
            (start-with-target item0 target)
            (update item0 1.0)
            (stop item0))
           ((= prev 0)
            (update item0 1.0)
            (stop item0))))
        ((and (= found 0) (= prev 1))
         ;;"reverse" noted to be buggy.
         (update item1 0.0)
         (stop item1)))
      (setf action (if (= found 0) item0 item1))
      (when (and (= found prev) (stopped-p action))
        (return-from update))
      (when (not (= found prev))
        (start-with-target action target))
      (update action new-time)
      (setf prev found))))

(defun sequence-2 (item0 item1)
  (assert item0)
  (assert item1)
  (flet ((get-duration (item)
           (let ((d (finite-time-action-duration item)))
             (if (= d 0.0) single-float-epsilon d))))
    (let ((duration (+ (get-duration item0)
                       (get-duration item1))))
      (setf duration (if (= duration 0.0) (* 2.0 single-float-epsilon) duration))
      (make-run-sequence :item0 item0
                         :item1 item1
                         :duration duration))))

(defun run-sequence (&rest items)
  (declare (dynamic-extent items))
  (assert items)
  (when (null (cdr items))
    (return-from run-sequence (first items)))
  (let ((prev (first items)))
    (dolist (item (rest items))
      (setf prev (sequence-2 prev item)))
    prev))


;; ======================================================================
;; easing actions

(defstruct (ease (:include finite-time-action))
  inner
  function)

(defmethod start-with-target ((self ease) node)
  (call-next-method)
  (start-with-target (ease-inner self) node))

(defmethod reset ((self ease))
  (call-next-method self)
  (reset (ease-inner self)))

(defmethod stop ((self ease))
  (call-next-method)
  (stop (ease-inner self)))

(defmethod update ((self ease) time)
  (update (ease-inner self)
          (funcall (ease-function self) time)))

(defvar *easing-functions* (make-hash-table :test 'eq))
(defvar *easing-function-functions* (make-hash-table :test 'eq))

(defun find-easing-function (keyword)
  (gethash keyword *easing-functions*))

(defmacro ease (name value)
  (let ((fname (gethash name *easing-function-functions*)))
    (unless fname (error "unkown easing function: ~S" name))
    `(,fname ,value)))

(macrolet ((defease (name (var) &body body)
             (let ((easefn (symbolicate '%ease- name))
                   (fname  (symbolicate 'ease- name))
                   (lookup-name (intern (symbol-name name) :keyword)))
               `(progn
                  (setf (gethash ,lookup-name *easing-functions*) ',fname)
                  (setf (gethash ,lookup-name *easing-function-functions*) ',easefn)
                  (defun ,easefn (,var) (coerce (progn,@body) 'single-float))
                  (defun ,fname (action)
                    (make-ease :duration (finite-time-action-duration action)
                               :function (function ,easefn)
                               :inner action))))))
  (defease linear (time) time)
  (defease in-sine (time)
    (1+ (* -1.0 (cos (* time (/ pi 2.0))))))
  (defease out-sine (time)
    (sin (* time (/ pi 2.0))))
  (defease in-out-sine (time)
    (* -0.5 (1- (cos (* pi time)))))
  (defease in-quad (time)
    (* time time))
  (defease out-quad (time)
    (* -1.0 time (- time 2.0)))
  (defease in-out-quad (time)
    (setf time (* 2.0 time))
    (if (< time 1.0)
        (* 0.5 time time)
        (progn
          (decf time)
          (* -0.5 (1- (* time (- time 2.0)))))))
  (defease in-cubic (time)
    (* time time time))
  (defease in-quart (time)
    (* time time time time))
  (defease in-quint (time)
    (* time time time time time))
  (defease in-quadratic (time)
    (expt time 2.0)))

(defmacro defact (name (&rest args) &body body)
  (with-gensyms (action)
    `(defun ,name (,@args &key ease)
       (let ((,action (progn ,@body)))
         (when ease
           (if-let (fn (find-easing-function ease))
             (setf ,action (funcall fn ,action))
             (error "unknown easing function ~S" ease)))
         ,action))))

;; ======================================================================
;; finite time actions

(defmacro make-property-lerp-action (name (&rest descs))
  (let ((target (symbolicate name '-target))
        (make (symbolicate 'make- name)))
    (labels ((init (desc)
               (symbolicate name '-initial-value- (first desc)))
             (tgt (desc)
               (symbolicate name '-target-value- (first desc)))
             (delta (desc)
               (symbolicate name '-delta- (first desc)))
             (relative? (desc)
               (not (null (getf desc :relative))))
             (slot-name (desc)
               (if (relative? desc)
                   (symbolicate 'delta- (first desc))
                   (symbolicate 'target-value- (first desc)))))
      `(progn
         (defstruct (,name (:include finite-time-action))
           ,@(loop for desc in descs append
                  (list
                   (symbolicate 'initial-value- (first desc))
                   (slot-name desc))))
         (defmethod start-with-target ((self ,name) target)
           (call-next-method)
           ,@(loop for desc in descs for init = (init desc)
                for property-name = (first desc)
                collect
                  `(setf (,init self) (,property-name target))))
         (defmethod reset ((self ,name))
           (call-next-method)
           ,@(loop for desc in descs for init = (init desc)
                for property-name = (first desc)
                collect
                  `(setf (,init self) (,property-name (,target self)))))
         (defmethod update ((self ,name) time)
           ,@(loop for desc in descs
                for init = (init desc) for tgt = (tgt desc)
                for delta = (delta desc)
                for property-name = (first desc)
                collect
                  (if (relative? desc)
                      `(setf (,property-name (,target self))
                             (+ (,init self) (* (,delta self) time)))
                      `(setf (,property-name (,target self))
                             (lerp time (,init self) (,tgt self))))))
         (defact ,name (duration ,@(mapcar 'second descs))
           (,make :duration duration
                  ,@(loop for desc in descs
                       for sym = (slot-name desc)
                       for kw = (intern (symbol-name sym) :keyword)
                       append (list kw (second desc)))))))))


(make-property-lerp-action move-by ((xmas.node:x x :relative t)
                                    (xmas.node:y y :relative t)))

(make-property-lerp-action move-by-x ((xmas.node:x x :relative t)))
(make-property-lerp-action move-by-y ((xmas.node:y y :relative t)))

(make-property-lerp-action move-to ((xmas.node:x x)
                                    (xmas.node:y y)))

(make-property-lerp-action scale-x-to ((xmas.node:scale-x scale-x)))
(make-property-lerp-action scale-y-to ((xmas.node:scale-y scale-y)))

(defstruct (delay (:include finite-time-action)))

(defmethod update ((self delay) time)
  (declare (ignore time)))

(defun delay (seconds)
  (make-delay :duration seconds))

(defstruct (rotate-by (:include finite-time-action))
  delta
  initial-rotation)

(defmethod start-with-target ((self rotate-by) target)
  (call-next-method)
  (setf (rotate-by-initial-rotation self)
        (xmas.node:rotation target)))

(defmethod reset ((self rotate-by))
  (call-next-method)
  (setf (rotate-by-initial-rotation self)
        (xmas.node:rotation (rotate-by-target self))))

(defmethod update ((self rotate-by) time)
  (with-struct (rotate-by- delta initial-rotation target) self
    (let* ((rotation (mod (+ initial-rotation (* time delta)) 360.0)))
      (setf (xmas.node:rotation target) rotation))))

(defact rotate-by (duration delta)
  (make-rotate-by :duration duration :delta delta))

(defstruct (fade-in (:include finite-time-action)))

(defmethod update ((self fade-in) time)
  (setf (xmas.node:opacity (fade-in-target self)) (clamp time 0.0 1.0)))

(defact fade-in (duration)
  (make-fade-in :duration duration))

(defstruct (fade-out (:include finite-time-action)))

(defmethod update ((self fade-out) time)
  (setf (xmas.node:opacity (fade-out-target self)) (clamp (- 1.0 time) 0.0 1.0)))

(defact fade-out (duration)
  (make-fade-out :duration duration))

(defstruct (tint-to (:include finite-time-action))
  start-r start-g start-b
  r g b)

(defmethod start-with-target ((self tint-to) target)
  (call-next-method)
  (let ((c (xmas.node:color target)))
    (with-struct (tint-to- start-r start-g start-b) self
      (setf start-r (svref c 0)
            start-g (svref c 1)
            start-b (svref c 2)))))

(defmethod reset ((self tint-to))
  (call-next-method)
  (let* ((target (tint-to-target self))
         (c (xmas.node:color target)))
    (with-struct (tint-to- start-r start-g start-b) self
      (setf start-r (svref c 0)
            start-g (svref c 1)
            start-b (svref c 2)))))

(defmethod update ((self tint-to) time)
  (let* ((target (tint-to-target self))
         (c (xmas.node:color target)))
    (with-struct (tint-to- start-r start-g start-b r g b) self
      (setf (svref c 0) (lerp time start-r r)
            (svref c 1) (lerp time start-g g)
            (svref c 2) (lerp time start-b b)))))

(defact tint-to (duration r g b)
  (make-tint-to :duration duration :r r :g g :b b))

(defun hsv-to-rgb (h s v)
  (let* ((chroma (* v s))
         (c chroma)
         (hprime (mod (/ h 60.0) 6.0))
         (x (* chroma (- 1.0 (abs (- (mod hprime 2.0) 1.0)))))
         (m (- v chroma)))
    (macrolet ((cases (&rest cases) 
                 `(cond ,@(loop for (lower upper r g b) in cases collect
                               `((and (<= ,lower hprime) (< hprime ,upper))
                                 (values (+ m ,r) (+ m ,g) (+ m ,b))))
                        (t (values m m m)))))
      (cases
       (0.0 1.0 c x 0.0)
       (1.0 2.0 x c 0.0)
       (2.0 3.0 0.0 c x)
       (3.0 4.0 0.0 x c)
       (4.0 5.0 x 0.0 c)
       (5.0 6.0 c 0.0 x)))))

(defstruct (hue-cycle (:include finite-time-action))
  color)

(defmethod start-with-target ((self hue-cycle) target)
  (call-next-method)
  (setf (hue-cycle-color self) (xmas.node:color target)))

(defmethod update ((self hue-cycle) time)
  (multiple-value-bind (r g b) (hsv-to-rgb (* time 360.0) 1.0 1.0)
    (let ((c (hue-cycle-color self)))
      (setf (svref c 0) r
            (svref c 1) g
            (svref c 2) b))))

(defact hue-cycle (duration)
  (make-hue-cycle :duration duration))

(defstruct (hue-cycle-with-offset (:include hue-cycle))
  offset
  saturation
  value)

(defmethod update ((self hue-cycle-with-offset) time)
  (let ((time (mod (+ time (hue-cycle-with-offset-offset self)) 1.0)))
    (multiple-value-bind (r g b)
        (hsv-to-rgb (* time 360.0)
                    (hue-cycle-with-offset-saturation self)
                    (hue-cycle-with-offset-value self))
      (let ((c (hue-cycle-color self)))
        (setf (svref c 0) r
              (svref c 1) g
              (svref c 2) b)))))

(defact hue-cycle-with-offset (duration offset &optional (saturation 1.0) (value 1.0))
  (make-hue-cycle-with-offset :duration duration :offset offset
                              :saturation saturation :value value))

(defstruct (blink (:include finite-time-action))
  blink-duration)

(defmethod update ((self blink) time)
  (setf (xmas.node:visible (blink-target self))
        (evenp (floor time (blink-blink-duration self)))))

(defmethod stop ((self blink))
  (call-next-method)
  (let ((it (blink-target self)))
    (setf (xmas.node:visible it) t)))

(defact blink (duration blink-duration)
  (make-blink :duration duration :blink-duration (/ blink-duration duration)))

(defstruct (lerp-slot-to (:include finite-time-action))
  slot-name
  initial-value
  target-value)

(defmethod start-with-target ((self lerp-slot-to) target)
  (call-next-method)
  (let ((slot (lerp-slot-to-slot-name self)))
    (setf (lerp-slot-to-initial-value self) (slot-value target slot))))

(defmethod reset ((self lerp-slot-to))
  (call-next-method)
  (let ((target (lerp-slot-to-target self))
        (slot (lerp-slot-to-slot-name self)))
    (setf (lerp-slot-to-initial-value self) (slot-value target slot))))

(defmethod update ((self lerp-slot-to) time)
  (let ((target (lerp-slot-to-target self))
        (slot (lerp-slot-to-slot-name self)))
    (setf (slot-value target slot)
          (lerp time
                (lerp-slot-to-initial-value self)
                (lerp-slot-to-target-value self)))))

(defact lerp-slot-to (duration slot-name target-value)
  (make-lerp-slot-to :duration duration
                     :slot-name slot-name
                     :target-value target-value))

;; ======================================================================
;; instant actions

(defstruct (instant-action (:include finite-time-action)))

(defmethod step-action ((self instant-action) dt)
  (declare (ignorable dt))
  (update self 1.0))

(defmethod update ((self instant-action) time)
  (declare (ignorable time))
  (stop self))

(defstruct (callfunc (:include instant-action))
  function
  args)

(defmethod update ((self callfunc) time)
  (declare (ignorable time))
  (call-next-method)
  (apply (callfunc-function self) (callfunc-args self)))

(defun callfunc (fn &rest args)
  (make-callfunc :function fn :args args))
