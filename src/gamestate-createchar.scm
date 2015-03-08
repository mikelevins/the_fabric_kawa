;;;; ***********************************************************************
;;;;
;;;; Name:          gamestate-createchar.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       Create a new character
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 CreateCharacterGameState)

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
;;; the CreateCharacterGameState class
;;; ---------------------------------------------------------------------

;;; CLASS CreateCharacterGameState
;;; ---------------------------------------------------------------------

(defclass CreateCharacterGameState (FabricGameState)
  (slots:)
  (methods:
   ((cleanup)
    (format #t "~%cleanup called for CreateCharacterGameState..."))
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (format #t "~%Preparing to attach CreateCharacterGameState..."))
   ((stateAttached mgr::AppStateManager)
    (format #t "~%CreateCharacterGameState attached..."))
   ((stateDetached mgr::AppStateManager)
    (format #t "~%CreateCharacterGameState detached..."))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (format #t "~%Cleaning up after detaching CreateCharacterGameState..."))))




