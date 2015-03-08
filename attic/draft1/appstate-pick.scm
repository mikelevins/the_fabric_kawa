;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-pick.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       pick or create a character to play
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

;;; NOTE: picking also means picking the node of the Fabric. If creating
;;; a new character the player must choose that character's starting
;;; node. If returning to an existing character, the player must choose
;;; which save node to activate (unless the player's character only has
;;; one, but that should be relatively rare)

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

