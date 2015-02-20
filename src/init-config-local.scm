;;;; ***********************************************************************
;;;;
;;;; Name:          config.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       startup parameters, local configuration
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 fabric-root
 server-host
 server-name
 server-port
 server-version)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This is the local version of the Fabric configuration file. It is
;;; loaded at client and server startup in the case where the client
;;; and the server run on the same machine (in other words, during
;;; development).

(define (fabric-root) (get-environment-variable "FABRIC_ROOT"))
(define (server-name) "The Fabric Server")
(define (server-version) 1)
(define (server-port) 6143)
(define (server-host) "localhost")


