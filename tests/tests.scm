;;; ***********************************************************************
;;;;
;;;; Name:          tests.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       simple utilities for testing
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(define-syntax test-case
  (syntax-rules (expecting:)
    ((test-case test-name
                body-expr ...
                expecting: test-form)
     (let ((result (try-catch (begin body-expr ...)
                              (err java.lang.Throwable
                                   (format #t "FAILURE: error ~a occurred in the body of test-case ~a"
                                           err 'test-name)
                                   #f))))
       (if (equal? result test-form)
           (format #t "~%test case ~a passed" 'test-name)
           (format #t "~%test case ~a FAILED!" 'test-name))))))

;;; (test-case foo ((i 101))
;;;            (+ i 1)
;;;            expecting 102)
