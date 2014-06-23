;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          config.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       code for reading configuration data
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export fabric-root)

(define fabric-root (make-parameter (get-environment-variable "FABRIC_ROOT")))
(define +server-name+ "The Fabric Server")
(define +server-version+ '(0 1 0))
(define +server-port+ 5110)
(define +server-address+ "localhost")

