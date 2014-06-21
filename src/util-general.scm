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

(module-export type-of get-resource identity bytes->long long->bytes
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
  (@ 'getClass thing))

(define (get-resource resname)
  (@ 'getResource (@ 'getContextClassLoader (Thread:currentThread)) resname))

(define (identity x) x)

(define (bytes->long b)
  (let ((len (length b)))
    (if (= len 8)
        (let loop ((i 0)
                   (result 0))
          (if (< i len)
              (loop (+ i 1)
                    (+ result (bitwise-arithmetic-shift-left (list-ref b i) (* i 8))))
              result))
        (error "bytes->long requires 8 bytes, but found" len))))

(define (long->bytes long)
  (let loop ((i 0)
             (bytes '()))
    (if (< i 8)
        (loop (+ i 1)
              (cons (bitwise-and (bitwise-arithmetic-shift-right long (* i 8))
                                 #b11111111)
                    bytes))
        (reverse bytes))))

(define (byte->bool-vector b)
  (list->vector (reverse (map (lambda (i)(bitwise-bit-set? b i))
                              (iota 8)))))

(define (string-split-at string index)
  (list (substring string 0 index)
        (substring string index (string-length string))))
