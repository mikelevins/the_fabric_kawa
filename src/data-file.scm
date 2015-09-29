;;;; ***********************************************************************
;;;;
;;;; Name:          data-file.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       data storage on local files
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 read-file
 write-sexp)

(import (class gnu.lists Pair))
(import (class java.io File))
(import (class java.lang String))

(define (write-sexp sexp path::String)
  (call-with-output-file path
      (lambda (out)
        (format out "~S" sexp))))

(define (read-file path::String #!key (default #f))
  (call-with-input-file path
      (lambda (in)
        (let ((file-data (read in)))
          (if (eqv? #!eof file-data)
              default
              file-data)))))


