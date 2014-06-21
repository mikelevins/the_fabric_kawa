;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          namegen.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the name generator
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export fabric-name-data fabric-name-bytes fabric-name-strings gen-name)

(require 'list-lib)
(require "util-java.scm")
(require "util-random.scm")
(require "util-lists.scm")
(require "util-general.scm")
(require "model-namegen-domains.scm")

(define-simple-class fabric-name ()
  (data type: long init-keyword: data:)
  ((getData) data))

(define (fabric-name-data nm::fabric-name)
  (@ 'getData nm))

(define (fabric-name-bytes nm)
  (long->bytes (fabric-name-data nm)))

(define (fabric-name-bit-patterns nm)
  (map (lambda (b)(format #f "~8,'0b" b))
       (fabric-name-bytes nm)))

(define (fabric-name-strings nm)
  (let* ((bytes (long->bytes (fabric-name-data nm)))
         (parts (map (lambda (b dom)(list-ref dom b))
                     bytes
                     (domain-name-lists))))
    (filter (lambda (p)(not (or (equal? p 0)
                                (equal? p ""))))
            parts)))

(define (gen-name)
  (let* ((bytes (map (lambda (i)(+ 1 (random-integer 255)))
                     (iota 8)))
         (name-len (+ 1 (random-integer 4)))
         (chosen-bytes (let loop ((in-bytes bytes))
                         (if (> (length in-bytes) name-len)
                             (loop (drop-any in-bytes))
                             in-bytes)))
         (zero-count (- 8 name-len))
         (zeros (list-fill zero-count 0))
         (long (bytes->long (shuffle (append chosen-bytes zeros)))))
    (if (=  long 0)
        (gen-name)
        (fabric-name data: long))))

(define (show-names n)
  (let loop ((i 0))
    (when (< i n)
      (let ((nm (gen-name)))
        (format #t "~%~S ~S"
                (fabric-name-data nm)
                (fabric-name-strings nm))
        (loop (+ i 1))))))


;;; (show-names 10)

