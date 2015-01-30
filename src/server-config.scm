;;;; ***********************************************************************
;;;;
;;;; Name:          server-config.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       server site-configuration parameters
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this file goes in the server process' working directory
;;; the fabric server reads it to determine server-startup
;;; parameters such as the server pathname and the pathname
;;; for server data files

(module-export fabric-data-root)

(define fabric-data-root (make-parameter (string-append (fabric-root) "share")))

