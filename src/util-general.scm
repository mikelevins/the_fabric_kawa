;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          util.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       general-purpose utilities
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export type-of get-resource identity bytes->integer integer->bytes
               byte->bool-vector string-split-at)

(require 'list-lib)
(require 'srfi-95) ; sorting
(require "util-java.scm")
(require "util-random.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias ArrayList java.util.ArrayList)
(define-private-alias Class java.lang.Class)
(define-private-alias Map com.github.krukow.clj_lang.PersistentHashMap)
(define-private-alias Thread java.lang.Thread)
(define-private-alias UUID java.util.UUID)

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(define (type-of thing)
  (*:getClass thing))

(define (get-resource resname)
  (*:getResource (*:getContextClassLoader (Thread:currentThread)) resname))

(define (identity x) x)

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

;; (define (long->bytes long)
;;   (let loop ((i 0)
;;              (bytes '()))
;;     (if (< i 8)
;;         (loop (+ i 1)
;;               (cons (bitwise-and (bitwise-arithmetic-shift-right long (* i 8))
;;                                  #b11111111)
;;                     bytes))
;;         (reverse bytes))))

              


(define (byte->bool-vector b)
  (list->vector (reverse (map (lambda (i)(bitwise-bit-set? b i))
                              (iota 8)))))

(define (string-split-at string index)
  (list (substring string 0 index)
        (substring string index (string-length string))))
