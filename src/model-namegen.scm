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

(module-export  fabric-name-bytes fabric-name-strings fabric-name-bit-patterns
                fabric-name-bytestrings gen-name)

(require 'list-lib)
(require "util-java.scm")
(require "syntax-classes.scm")
(require "util-random.scm")
(require "util-lists.scm")
(require "util-general.scm")
(require "model-namegen-domains.scm")

(defclass fabric-name ()
  (slots: (data type: gnu.math.IntNum getter: getData setter: setData))
  (methods: ((*init* num)(set! data num))))

(define (fabric-name-bytes nm)
  (integer->bytes (*:getData nm)))

(define (bytes->strings bytes)
  (map (lambda (b)(format #f "~2,'0x" b))
       bytes))

(define (fabric-name-bytestrings nm)
  (bytes->strings (fabric-name-bytes nm)))

(define (fabric-name-bit-patterns nm)
  (map (lambda (b)(format #f "~8,'0b" b))
       (fabric-name-bytes nm)))

(define (fabric-name-strings nm)
  (let* ((bytes (integer->bytes (*:getData nm)))
         (parts (map (lambda (b dom)(list-ref dom b))
                     bytes
                     (domain-name-lists))))
    (filter (lambda (p)(not (or (equal? p 0)
                                (equal? p ""))))
            parts)))

(define (gen-bytes n)
  (map (lambda (i)(random-integer 255))
       (iota n)))

(define (gen-name)
  (let* ((bytes (gen-bytes (choose-any '(1 2 3 4))))
         (zeros (list-fill (- 8 (length bytes)) 0))
         (num (apply bytes->integer (shuffle (append bytes zeros)))))
    (if (zero? num)
        (gen-name)
        (fabric-name num))))

