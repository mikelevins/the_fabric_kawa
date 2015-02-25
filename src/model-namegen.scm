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
 fabric-name-bit-patterns
 fabric-name-bytes
 fabric-name-bytestrings
 fabric-name-strings
 gen-name
 name-part->domain-and-index)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file implements the Fabric name generator, which can
;;; construct arbitrary Fabric names from the data in the
;;; data-names.scm file. It also provides utilities for converting
;;; among the different representations of a Fabric name: 64-bit
;;; integer, a sequence of up to 8 text strings, bit strings, and
;;; hexadecimal byte strings.

(require 'list-lib)
(require "util-java.scm")
(require "util-bytes.scm")
(require "syntax-classes.scm")
(require "util-bytes.scm")
(require "util-random.scm")
(require "util-lists.scm")
(require "data-names.scm")


(import-as String java.lang.String)


;;; CLASS FabricName
;;; ---------------------------------------------------------------------
;;; the class of Fabric names

(defclass FabricName ()
  (slots: (data type: gnu.math.IntNum getter: getData setter: setData))
  (methods: ((*init* num)(set! data num))))


;;; (fabric-name-bytes nm :: FabricName)
;;; ---------------------------------------------------------------------
;;; returns a list of bytes corresponding to the integer value of the
;;; Fabric name

(define (fabric-name-bytes nm::FabricName)
  (integer->bytes (*:getData nm)))


;;; (fabric-name-bytestrings nm)
;;; ---------------------------------------------------------------------
;;; returns a list of hexadecimal bytestrings corresponding
;;; to the Fabric name

(define (fabric-name-bytestrings nm::FabricName)
  (bytes->strings (fabric-name-bytes nm)))


;;; (fabric-name-bit-patterns nm)
;;; ---------------------------------------------------------------------
;;; returns a string that displays the pattern of bits in the Fabric name

(define (fabric-name-bit-patterns nm::FabricName)
  (map (lambda (b)(format #f "~8,'0b" b))
       (fabric-name-bytes nm)))


;;; (fabric-name-strings nm :: FabricName)
;;; ---------------------------------------------------------------------
;;; returns the text version of the Fabric name as a list of strings,
;;; with empty strings omitted

(define (fabric-name-strings nm::FabricName)
  (let* ((bytes (integer->bytes (*:getData nm)))
         (parts (map (lambda (b dom)(list-ref dom b))
                     bytes
                     (name-domains))))
    (filter (lambda (p)(not (or (equal? p 0)
                                (equal? p ""))))
            parts)))

(define (name-part->domain-index domain part)
  (position-if (lambda (it)(equal? part it))
               domain))

(define (name-part->domain-and-index part)
  (let* ((domains (name-domains))
         (domain-count (length domains)))
    (let loop ((i 0))
      (if (< i domain-count)
          (let* ((domain (list-ref domains i))
                 (pos (name-part->domain-index domain part)))
            (if pos
                (values i pos)
                (loop (+ 1 i))))
          (values #f #f)))))

;;; (gen-name)
;;; ---------------------------------------------------------------------
;;; generates a random Fabric name

(define (gen-name)
  (let* ((bytes (gen-bytes (choose-any '(1 2 3 4))))
         (zeros (list-fill (- 8 (length bytes)) 0))
         (num (apply bytes->integer (shuffle (append bytes zeros)))))
    (if (zero? num)
        (gen-name)
        (FabricName num))))

