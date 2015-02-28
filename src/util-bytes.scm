;;;; ***********************************************************************
;;;;
;;;; Name:          util-bytes.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       utilities for working with bytes
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 byte->bits
 bytes->integer
 bytes->strings
 gen-bytes
 integer->bits
 integer->bytes)

(require 'srfi-60)
(require 'list-lib)
(require "util-random.scm")

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this file provides a set of byte-handling utilities.

;;; (bytes->integer a b c d e f g h)
;;; ---------------------------------------------------------------------
;;; returns the integer corresponding to the given sequence of input bytes

(define (bytes->integer a b c d e f g h)
  (gnu.math.IntNum:make (int[] a b c d e f g h)))


;;; (bytes->strings bytes)
;;; ---------------------------------------------------------------------
;;; returns a sequence of strings, each displaying the hexadecimal
;;; value of the corresponding byte in _bytes_

(define (bytes->strings bytes)
  (map (lambda (b)(format #f "~2,'0x" b))
       bytes))


;;; (gen-bytes n)
;;; ---------------------------------------------------------------------
;;; returns a sequence of _n_ randomly-chosen bytes

(define (gen-bytes n)
  (map (lambda (i)(random-integer 255))
       (iota n)))

;;; (integer->bytes num::gnu.math.IntNum)
;;; ---------------------------------------------------------------------
;;; returns a sequence of bytes that corresponds to the value of the
;;; integer _num_
;;; BUG: assumes num can never be more than 64 bits

(define (integer->bytes num::gnu.math.IntNum)
  (let ((bytes num:words))
    (if (eq? bytes #!null)
        (let ((val num))
          (list (bitwise-and (bitwise-arithmetic-shift-right val (* 0 8)) #b11111111)
                (bitwise-and (bitwise-arithmetic-shift-right val (* 1 8)) #b11111111)
                (bitwise-and (bitwise-arithmetic-shift-right val (* 2 8)) #b11111111)
                (bitwise-and (bitwise-arithmetic-shift-right val (* 3 8)) #b11111111)
                (bitwise-and (bitwise-arithmetic-shift-right val (* 4 8)) #b11111111)
                (bitwise-and (bitwise-arithmetic-shift-right val (* 5 8)) #b11111111)
                (bitwise-and (bitwise-arithmetic-shift-right val (* 6 8)) #b11111111)
                (bitwise-and (bitwise-arithmetic-shift-right val (* 7 8)) #b11111111)))
        (let* ((byte-count num:ival)
               (indexes (iota byte-count)))
          (map (lambda (i)(bytes i))
               indexes)))))

(define (byte->bits b)
  (let ((indexes (iota 8)))
    (reverse (map (lambda (i)(logbit? i b))
                  indexes))))

(define (integer->bits num::gnu.math.IntNum)
  (let* ((bytes (integer->bytes num))
         (bitlists (map byte->bits bytes)))
    (apply append bitlists)))
