;;; ***********************************************************************
;;;;
;;;; Name:          syntax-time.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       syntax definitions for timing execution
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 time)

(define-syntax time
  (syntax-rules ()
    ((time expr)
     (let ((start-jiffy (current-jiffy)))
       (let ((result expr))
         (let* ((end-jiffy (current-jiffy))
                (diff (exact->inexact (- end-jiffy start-jiffy))))
           (format #t "~%elapsed time for expression:~%~S~%~%~D seconds~%"
                   expr (/ diff (jiffies-per-second)))
           result))))))


