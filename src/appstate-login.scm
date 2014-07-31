;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-login.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       log in to the Fabric server
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export )

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)

;;; ---------------------------------------------------------------------
;;; the LoginAppState class
;;; ---------------------------------------------------------------------

(defclass LoginAppState (AbstractAppState)
  )
