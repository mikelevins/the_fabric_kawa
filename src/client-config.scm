;;;; ***********************************************************************
;;;;
;;;; Name:          client-config.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       client startup parameters
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export fabric-root server-name server-version server-port server-host)

;;; ---------------------------------------------------------------------
;;; networked configuration
;;; ---------------------------------------------------------------------

;; (define (fabric-root) (get-environment-variable "FABRIC_ROOT"))
;; (define (server-name) "The Fabric Server")
;; (define (server-version) 2)
;; (define (server-port) 6143)
;; (define (server-host) "explorersguild.com")

;;; ---------------------------------------------------------------------
;;; local configuration
;;; ---------------------------------------------------------------------

(define (fabric-root) (get-environment-variable "FABRIC_ROOT"))
(define (server-name) "The Fabric Server")
(define (server-version) 2)
(define (server-port) 6143)
(define (server-host) "localhost")

