;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-game.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       common superclass of all Fbric game states
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 GameState)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------

;;; a GameState is an AppState that represents one of the Fabric
;;; client's major modes: login, create a character, pick a character,
;;; or play. GameState defines a small set of common slots and methods
;;; that all Fabric GameStates must implement

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

;;; ---------------------------------------------------------------------
;;; the GameState class
;;; ---------------------------------------------------------------------

;;; CLASS GameState
;;; ---------------------------------------------------------------------

(defclass GameState (AbstractAppState)
  (slots:
   (app init-form: #!null getter: getApp setter: setApp)
   (initialized init-form: #F getter: getInitialized setter: setInitialized))
  (methods:
   ((cleanup)
    (error "cleanup must be implemented in subclasses of GameState"))
   ((isEnabled)
    (error "isEnabled must be implemented in subclasses of GameState"))
   ((isInitialized) initialized)
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (error "prepareToAttach must be implemented in subclasses of GameState"))
   ((stateAttached mgr::AppStateManager)
    (error "stateAttached must be implemented in subclasses of GameState"))
   ((stateDetached mgr::AppStateManager)
    (error "stateDetached must be implemented in subclasses of GameState"))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (error "cleanupDetached must be implemented in subclasses of GameState"))))




