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

(module-export fabric-root database-root)

(define fabric-root (make-parameter (get-environment-variable "FABRIC_ROOT")))
(define database-root (make-parameter (get-environment-variable "DB_ROOT")))


