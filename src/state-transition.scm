;;;; ***********************************************************************
;;;;
;;;; Name:          state-transition.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the neutral state between Fabric app states
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 TransitionState
 make-transition-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require client-state)
(require client-class)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app Application))
(import (class com.jme3.app.state AppStateManager))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; the client transit AppState class
;;; ---------------------------------------------------------------------

;;; CLASS TransitionState
;;; ---------------------------------------------------------------------

(define-simple-class TransitionState (FabricClientState)
  ;; slots
  (initialized? init: #f)
  ;; methods
  ((cleanup) (%transition-state-cleanup (this)))
  ((isEnabled) (%transition-state-enabled? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%transition-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%transition-state-detached (this) state-manager))
  ;; init
  ((initialize) (%transition-state-initialize (this)))
  ((isInitialized) (%transition-state-initialized? (this))))

(define (%transition-state-cleanup state::TransitionState) #!void)
(define (%transition-state-enabled? state::TransitionState) #t)
(define (%transition-state-attached state::TransitionState state-manager) #!void)
(define (%transition-state-detached state::TransitionState state-manager) #!void)
(define (%transition-state-initialize state::TransitionState) #!void)
(define (%transition-state-initialized? state::TransitionState) #t)

(define (make-transition-state client::Application)
  (let ((state (TransitionState)))
    (set! state:client client)
    state))

