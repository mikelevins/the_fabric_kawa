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
 blank-fabric-name
 FabricName
 fabric-name-bit-patterns
 fabric-name-bytes
 fabric-name-bytestrings
 fabric-name-strings
 gen-name
 name-part->domain-and-index
 strings->fabric-name
 update-fabric-name)

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
  (let* ((data-bytes (integer->bytes (*:getData nm)))
         (data-count (length data-bytes))
         (result-bytes (list-fill 8 0)))
    (append data-bytes
            (drop result-bytes data-count))))


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
  (let* ((bytes (fabric-name-bytes nm))
         (parts (map (lambda (b dom)(list-ref dom b))
                     bytes
                     (name-domains))))
    parts))

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

(define (blank-fabric-name)
  (FabricName 0))

(define (strings->fabric-name strings)
  (let* ((bytes (map (lambda (domain part)(name-part->domain-index domain part))
                     (name-domains)
                     strings))
         (data (apply bytes->integer bytes)))
    (FabricName data)))

(define (update-fabric-name current-name::FabricName domain-index part-index)
  (let* ((old-strings (fabric-name-strings current-name))
         (part-domain (list-ref (name-domains) domain-index))
         (new-part (list-ref part-domain part-index))
         (new-strings (replace-element old-strings domain-index new-part)))
    (strings->fabric-name new-strings)))

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

