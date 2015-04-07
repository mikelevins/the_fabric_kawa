;;;; ***********************************************************************
;;;;
;;;; Name:          state-play.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading state-play.scm")

(module-export
 PlayState
 make-play-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "data-nodes.scm")
(require "syntax-classes.scm")
(require "client-state.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Application com.jme3.app.Application)
(import-as AppStateManager com.jme3.app.state.AppStateManager)

;;; ---------------------------------------------------------------------
;;; the client play AppState class
;;; ---------------------------------------------------------------------

;;; CLASS PlayState
;;; ---------------------------------------------------------------------

(defclass PlayState (FabricClientState)
  (slots:
   (node-name init-form: #f getter: getNodeName setter: setNodeName)
   (initialized? init-form: #f getter: getInitialized setter: setInitialized)) 
  (methods:
   ((cleanup) (%play-state-cleanup (this)))
   ((initialize) (%play-state-initialize (this)))
   ((isEnabled) (%play-state-enabled? (this)))
   ((isInitialized) (%play-state-initialized? (this)))
   ((stateAttached state-manager::AppStateManager)
    (%play-state-attached (this) state-manager))
   ((stateDetached state-manager::AppStateManager)
    (%play-state-detached (this) state-manager))))

(define (%play-state-cleanup state::PlayState) #!void)
(define (%play-state-initialize state::PlayState) #!void)
(define (%play-state-enabled? state::PlayState) #t)
(define (%play-state-initialized? state::PlayState) #t)
(define (%play-state-attached state::PlayState manager::AppStateManager) #!void)
(define (%play-state-detached state::PlayState manager::AppStateManager) #!void)

(define (make-play-state client::Application node-name)
  (let ((state (PlayState)))
    (*:setClient state client)
    (*:setNodeName state node-name)
    state))
