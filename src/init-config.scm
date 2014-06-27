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

(module-export fabric-version fabric-root server-name server-version server-port server-host)

(define fabric-version (make-parameter "0.1.0d174"))
(define fabric-root (make-parameter (get-environment-variable "FABRIC_ROOT")))
(define server-name (make-parameter "The Fabric Server"))
(define server-version (make-parameter 1))
(define server-port (make-parameter 6143))
(define server-host (make-parameter "localhost"))


