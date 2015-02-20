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
 fabric-name
 fabric-name-bit-patterns
 fabric-name-bytes
 fabric-name-bytestrings
 fabric-name-strings
 gen-name)

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
(require "util-random.scm")
(require "util-lists.scm")
(require "data-names.scm")

;;; 
;;; ---------------------------------------------------------------------
;;; 

(defclass fabric-name ()
  (slots: (data type: gnu.math.IntNum getter: getData setter: setData))
  (methods: ((*init* num)(set! data num))))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (fabric-name-bytes nm :: fabric-name)
  (integer->bytes (*:getData nm)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (bytes->strings bytes)
  (map (lambda (b)(format #f "~2,'0x" b))
       bytes))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (fabric-name-bytestrings nm)
  (bytes->strings (fabric-name-bytes nm)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (fabric-name-bit-patterns nm)
  (map (lambda (b)(format #f "~8,'0b" b))
       (fabric-name-bytes nm)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (fabric-name-strings nm :: fabric-name)
  (let* ((bytes (integer->bytes (*:getData nm)))
         (parts (map (lambda (b dom)(list-ref dom b))
                     bytes
                     (name-domains))))
    (filter (lambda (p)(not (or (equal? p 0)
                                (equal? p ""))))
            parts)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (gen-bytes n)
  (map (lambda (i)(random-integer 255))
       (iota n)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (gen-name)
  (let* ((bytes (gen-bytes (choose-any '(1 2 3 4))))
         (zeros (list-fill (- 8 (length bytes)) 0))
         (num (apply bytes->integer (shuffle (append bytes zeros)))))
    (if (zero? num)
        (gen-name)
        (fabric-name num))))

