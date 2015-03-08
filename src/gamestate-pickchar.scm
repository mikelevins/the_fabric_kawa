;;;; ***********************************************************************
;;;;
;;;; Name:          gamestate-login.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       Choose a character to play
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 PickCharacterGameState)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")
(require "appstate-gamestate.scm")
(require "client-main.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AppStateManager com.jme3.app.state.AppStateManager)

;;; ---------------------------------------------------------------------
;;; the PickCharacterGameState class
;;; ---------------------------------------------------------------------

;;; CLASS PickCharacterGameState
;;; ---------------------------------------------------------------------

(defclass PickCharacterGameState (FabricGameState)
  (slots:)
  (methods:
   ((cleanup)
    (format #t "~%cleanup called for PickCharacterGameState..."))
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (format #t "~%Preparing to attach PickCharacterGameState..."))
   ((stateAttached mgr::AppStateManager)
    (format #t "~%PickCharacterGameState attached..."))
   ((stateDetached mgr::AppStateManager)
    (format #t "~%PickCharacterGameState detached..."))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (format #t "~%Cleaning up after detaching PickCharacterGameState..."))))




