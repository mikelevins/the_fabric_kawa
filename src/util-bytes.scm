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
 bytes->integer
 bytes->strings
 gen-bytes
 integer->bytes)

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

(define (integer->bytes num::gnu.math.IntNum)
  (let ((bytes num:words))
    (if (eq? bytes #!null)
        (let ((val num:ival))
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
