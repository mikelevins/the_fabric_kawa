;;;; ***********************************************************************
;;;;
;;;; Name:          client-presentation.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric client presentation server
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export )

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; the Fabric client uses a separate presentation server
;;; in order to decouple rendering, presentation, and user-event handling
;;; from the rest of the program, and to make it possible to write
;;; tools and other useful programs in languages other than Java or Kawa

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------


;;; ---------------------------------------------------------------------
;;; Presentation server
;;; ---------------------------------------------------------------------
;;; the presentation server creates and manages windows and OpenGL scene
;;; graphs. It responds to user events and presents a 9P filesystem
;;; that local client programs can use to interact with the presentation
;;; server.
