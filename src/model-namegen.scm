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

(module-export
 FabricName
 blank-fabric-name
 fabric-name=?
 fabric-name->string
 fabric-name->bit-patterns
 fabric-name->bits
 fabric-name->bytestrings
 fabric-name->strings
 generate-fabric-name
 generate-mech-name
 random-fabric-name
 )

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file implements the Fabric name generator, which can
;;; construct arbitrary Fabric names from the data in the
;;; data-names.scm file. It also provides utilities for converting
;;; among the different representations of a Fabric name: 64-bit
;;; integer, a sequence of up to 8 text strings, bit strings, and
;;; hexadecimal byte strings.

(require 'srfi-60)
(require 'list-lib)
(require util-bytes)
(require util-random)
(require util-lists)
(require data-names)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; CLASS FabricName
;;; ---------------------------------------------------------------------

(define-simple-class FabricName ()
  (data type: int[])
  ((*init* bytes::int[])(set! data bytes))
  ((*init*) (set! data (apply int[] '(0 0 0 0 0 0 0 0)))))

(define (fabric-name=? fname1::FabricName fname2::FabricName)
  (equal? fname1:data fname2:data))

;;; (fabric-name->bits nm :: FabricName)
;;; ---------------------------------------------------------------------
;;; returns a list of bits corresponding to the integer value of the
;;; Fabric name

(define (fabric-name->bits nm::FabricName)
  (let* ((bits (apply append (map byte->bits nm:data)))
         (bit-count (length bits)))
    (append bits
            (list-fill (- 64 bit-count) #f))))

;;; (fabric-name->bytestrings nm)
;;; ---------------------------------------------------------------------
;;; returns a list of hexadecimal bytestrings corresponding
;;; to the Fabric name

(define (fabric-name->bytestrings nm::FabricName)
  (bytes->strings nm:data))


;;; (fabric-name->bit-patterns nm)
;;; ---------------------------------------------------------------------
;;; returns a string that displays the pattern of bits in the Fabric name

(define (fabric-name->bit-patterns nm::FabricName)
  (map (lambda (b)(format #f "~8,'0b" b))
       nm:data))

;;; (fabric-name->strings nm :: FabricName)
;;; ---------------------------------------------------------------------
;;; returns the text version of the Fabric name as a list of strings,
;;; with empty strings omitted

(define (fabric-name->strings nm::FabricName)
  (let* ((bytes nm:data)
         (parts (map (lambda (b dom)(list-ref dom b))
                     bytes
                     (name-domains))))
    parts))

(define (fabric-name->string nm::FabricName)
  (let ((parts (fabric-name->strings nm)))
    (apply string-append
           (interpose " "
                      (filter (lambda (x)(not (string=? x "")))
                              parts)))))

(define (blank-fabric-name)
  (FabricName (int[] 0 0 0 0 0 0 0 0)))

(define (random-fabric-name)
  (FabricName (apply int[] (gen-bytes 8))))

;;; (generate-fabric-name #!key (part-count 3))
;;; ---------------------------------------------------------------------
;;; generate and return a new random Fabric name with part-count
;;; non-blank parts

(define (generate-fabric-name #!key (part-count 3))
  (let* ((picked-indexes (let loop ((count part-count)
                                    (result '()))
                           (if (<= count 0)
                               result
                               (loop (- count 1)
                                     (cons (+ 1 (random-integer 255))
                                           result)))))
         (indexes (shuffle (append picked-indexes
                                   (list-fill (- 8 part-count) 0)))))
    (FabricName (apply int[] indexes))))

(define (generate-mech-name)
  (let* ((num-byte (+ 1 (random-integer 254)))
         (surname-byte (+ 1 (random-integer 254)))
         (name-index (choose-any '(0 2 4 5 6 7)))
         (name-byte (+ 1 (random-integer 254)))
         (name-domain (list-ref (name-domains) name-index)))
    (list (list-ref name-domain name-byte)
          num-byte
          (list-ref (domain3) surname-byte))))
