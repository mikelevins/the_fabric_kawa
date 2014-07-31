;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-pick.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       pick or create a character to play
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
;;; the PickAppState class
;;; ---------------------------------------------------------------------

(defclass PickAppState (AbstractAppState)
  )
