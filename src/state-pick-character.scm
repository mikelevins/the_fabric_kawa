;;;; ***********************************************************************
;;;;
;;;; Name:          state-pick-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading state-pick-character.scm")

(module-export
 PickCharacterState
 make-pick-character-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "data-nodes.scm")
(require "client-state.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Application com.jme3.app.Application)
(import-as AppStateManager com.jme3.app.state.AppStateManager)

;;; ---------------------------------------------------------------------
;;; the client character-creator AppState class
;;; ---------------------------------------------------------------------

;;; CLASS PickCharacterState
;;; ---------------------------------------------------------------------

(define-simple-class PickCharacterState (FabricClientState)
  ;; slots
  (initialized? init: #f)
  ((getInitialized) initialized?)
  ((setInitialized newstate) (set! initialized? newstate))
  ;; methods
  ((cleanup) (%pick-character-state-cleanup (this)))
  ((isEnabled) (%pick-character-state-enabled? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%pick-character-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%pick-character-state-detached (this) state-manager))
  ;; init
  ((initialize) (%pick-character-state-initialize (this)))
  ((isInitialized) (%pick-character-state-initialized? (this))))

(define (%pick-character-state-cleanup state::PickCharacterState)
  (format #t "~%%pick-character-state-cleanup called"))

(define (%pick-character-state-initialize state::PickCharacterState)
  (format #t "~%%pick-character-state-initialize called"))

(define (%pick-character-state-enabled? state::PickCharacterState) #t)

(define (%pick-character-state-initialized? state::PickCharacterState) #t)

(define (%pick-character-state-attached state::PickCharacterState manager::AppStateManager)
  (format #t "~%%pick-character-state-attached called"))

(define (%pick-character-state-detached state::PickCharacterState manager::AppStateManager)
  (format #t "~%%pick-character-state-detached called"))

(define (make-pick-character-state client::Application)
  (let ((state (PickCharacterState)))
    (*:setClient state client)
    state))
