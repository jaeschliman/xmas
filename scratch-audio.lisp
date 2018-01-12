;; ccl doesn't yet support AVFoundation out of the box.
(progmain ()
  (objc:load-framework "QTKit" "qtkit"))

(defvar *my-sound*)
(progmain ()
  (ccl::with-autoreleased-nsstring (s (princ-to-string (truename "./res/scream.mp3")))
    (setf *my-sound* 
          (#/initWithFile:error: (#/alloc ns:q-t-movie) s ccl:+null-ptr+))))

(progmain ()
  (#/play *my-sound*))
(progmain ()
  (#/stop *my-sound*))
