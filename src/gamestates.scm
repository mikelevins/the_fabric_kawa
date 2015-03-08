;;;; ***********************************************************************
;;;;
;;;; Name:          gamestates.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       Fabric game states
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 CreateCharacterGameState
 FabricGameState
 LoginGameState
 PickCharacterGameState
 PlayGameState)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------

;;; a FabricGameState is an AppState that represents one of the Fabric
;;; client's major modes: login, create a character, pick a character,
;;; or play. FabricGameState defines a small set of common slots and methods
;;; that all Fabric FabricGameStates must implement

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")
(require "client-main.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as AppStateManager com.jme3.app.state.AppStateManager)


;;; =====================================================================
;;; CLASS FabricGameState
;;; =====================================================================

(defclass FabricGameState (AbstractAppState)
  (slots:
   (app init-form: #!null getter: getApp setter: setApp)
   (initialized init-form: #F getter: getInitialized setter: setInitialized))
  (methods:
   ((cleanup)
    (error "cleanup must be implemented in subclasses of FabricGameState"))
   ((isEnabled)
    (error "isEnabled must be implemented in subclasses of FabricGameState"))
   ((isInitialized) initialized)
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (error "prepareToAttach must be implemented in subclasses of FabricGameState"))
   ((stateAttached mgr::AppStateManager)
    (error "stateAttached must be implemented in subclasses of FabricGameState"))
   ((stateDetached mgr::AppStateManager)
    (error "stateDetached must be implemented in subclasses of FabricGameState"))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (error "cleanupDetached must be implemented in subclasses of FabricGameState"))))


;;; =====================================================================
;;; CLASS CreateCharacterGameState
;;; =====================================================================

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


;;; =====================================================================
;;; CLASS LoginGameState
;;; =====================================================================

(defclass LoginGameState (FabricGameState)
  (slots:)
  (methods:
   ((cleanup)
    (format #t "~%cleanup called for LoginGameState..."))
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (format #t "~%Preparing to attach LoginGameState..."))
   ((stateAttached mgr::AppStateManager)
    (format #t "~%LoginGameState attached..."))
   ((stateDetached mgr::AppStateManager)
    (format #t "~%LoginGameState detached..."))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (format #t "~%Cleaning up after detaching LoginGameState..."))))


;;; =====================================================================
;;; CLASS PickCharacterGameState
;;; =====================================================================

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


;;; =====================================================================
;;; CLASS PlayGameState
;;; =====================================================================

(defclass PlayGameState (FabricGameState)
  (slots:)
  (methods:
   ((cleanup)
    (format #t "~%cleanup called for PlayGameState..."))
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (format #t "~%Preparing to attach PlayGameState..."))
   ((stateAttached mgr::AppStateManager)
    (format #t "~%PlayGameState attached..."))
   ((stateDetached mgr::AppStateManager)
    (format #t "~%PlayGameState detached..."))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (format #t "~%Cleaning up after detaching PlayGameState..."))))
