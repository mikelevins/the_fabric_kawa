;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-play.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the appstate for a player in the game world
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
;;; the NodeAppState class
;;; ---------------------------------------------------------------------

(defclass PlayAppState (AbstractAppState)
  )
