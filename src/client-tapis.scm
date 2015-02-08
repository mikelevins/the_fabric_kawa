;;;; ***********************************************************************
;;;;
;;;; Name:          client-tapis.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the fabric client presentation server
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export TapisServer)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; tapis is a presentation server that uses the 9P protocol to
;;; present its services to client programs, whether in-process or
;;; running in separate processes.

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ProcessFile j9p.ns.handlers.ProcessFile)

;;; ---------------------------------------------------------------------
;;; tapis
;;; ---------------------------------------------------------------------

(defclass TapisServer (ProcessFile:Listener)
  (slots:
   (app init-form: #!null getter: getApp setter: setApp))
  (methods:
   ((asInput bytes::byte[] offset::long) 0)
   ((getOutput offset::long how-many::int) #!null)))

