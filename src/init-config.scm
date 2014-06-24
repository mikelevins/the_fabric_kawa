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

(module-export fabric-root server-name server-version server-port server-address)

(define fabric-root (make-parameter (get-environment-variable "FABRIC_ROOT")))
(define server-name (make-parameter "The Fabric Server"))
(define server-version (make-parameter 1))
(define server-port (make-parameter 6143))
(define server-address (make-parameter "localhost"))


