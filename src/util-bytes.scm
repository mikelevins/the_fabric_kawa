;;;; ***********************************************************************
;;;;
;;;; Name:          util-bytes.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       utilities for working with bytes
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export bytes->integer integer->bytes)

(require 'list-lib)

;;; ---------------------------------------------------------------------
;;; bytes utilities
;;; ---------------------------------------------------------------------

(define (bytes->integer a b c d e f g h)
  (gnu.math.IntNum:make (int[] a b c d e f g h)))

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
