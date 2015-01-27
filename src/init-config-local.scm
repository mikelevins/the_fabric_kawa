;;;; ***********************************************************************
;;;;
;;;; Name:          config.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       startup parameters, local configuration
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export fabric-root server-name server-version server-port server-host)

(define (fabric-root) (get-environment-variable "FABRIC_ROOT"))
(define (server-name) "The Fabric Server")
(define (server-version) 1)
(define (server-port) 6143)
(define (server-host) "localhost")


