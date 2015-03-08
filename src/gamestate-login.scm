;;;; ***********************************************************************
;;;;
;;;; Name:          gamestate-login.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       log in to the Fabric
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 LoginGameState)

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
;;; the LoginGameState class
;;; ---------------------------------------------------------------------

;;; CLASS LoginGameState
;;; ---------------------------------------------------------------------

(defclass LoginGameState (FabricGameState)
  (slots:)
  (methods:
   ((cleanup)
    (format #t "cleanup called for LoginGameState..."))
   ((isEnabled) #t)
   ((isInitialized) initialized)
   ((prepareToAttach mgr::AppStateManager client::FabricClient)
    (format #t "Preparing to attach LoginGameState..."))
   ((stateAttached mgr::AppStateManager)
    (format #t "LoginGameState attached..."))
   ((stateDetached mgr::AppStateManager)
    (format #t "LoginGameState detached..."))
   ((cleanupDetached mgr::AppStateManager client::FabricClient)
    (format #t "Cleaning up after detaching LoginGameState..."))))




