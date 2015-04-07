;;;; ***********************************************************************
;;;;
;;;; Name:          client-state-pick-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading client-state-pick-character.scm")

(module-export
 PickCharacterClientState
 make-pick-character-state)

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
;;; the client character-creator AppState class
;;; ---------------------------------------------------------------------

;;; CLASS PickCharacterClientState
;;; ---------------------------------------------------------------------

(defclass PickCharacterClientState (FabricClientState)
  (slots:
   (initialized? init-form: #f getter: getInitialized setter: setInitialized)) 
  (methods:
   ((cleanup) (%pick-character-client-state-cleanup (this)))
   ((initialize) (%pick-character-client-state-initialize (this)))
   ((isEnabled) #t)
   ((isInitialized) (*:getInitialized (this)))
   ((stateAttached state-manager::AppStateManager)
    (%pick-character-client-state-attached (this) state-manager))
   ((stateDetached state-manager::AppStateManager)
    (%pick-character-client-state-detached (this) state-manager))))

(define (%pick-character-client-state-cleanup state::PickCharacterClientState)
  #!void)

(define (%pick-character-client-state-initialize state::PickCharacterClientState)
  #!void)

(define (%pick-character-client-state-attached state::PickCharacterClientState manager::AppStateManager)
  #!void)

(define (%pick-character-client-state-detached state::PickCharacterClientState manager::AppStateManager)
  #!void)

(define (make-pick-character-state client::Application)
  (let ((state (PickCharacterClientState)))
    (*:setClient state client)
    state))
