;;;; ***********************************************************************
;;;;
;;;; Name:          state-transition.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the transition AppState
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 TransitionState
 )

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require data-assets)
(require state)
(require client)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app.state AbstractAppState AppStateManager))

;;; ---------------------------------------------------------------------
;;; TransitionState
;;; ---------------------------------------------------------------------

(define (%transition-state-cleanup state::FabricClientState)
  (format #t "~%TransitionState cleaned up"))

(define (%transition-state-initialize state::FabricClientState)
  (format #t "~%TransitionState initialized")
  (set! state:state-initialized? #t))

(define (%transition-state-enabled? state::FabricClientState) #t)
(define (%transition-state-initialized? state::FabricClientState) state:state-initialized?)
(define (%transition-state-attached state::FabricClientState manager::AppStateManager) #!void)
(define (%transition-state-detached state::FabricClientState manager::AppStateManager) #!void)

(define (%transition-state-handle-analog-event state name value tpf)
  ;;; TODO: implement event handling
  #!void)

(define (%transition-state-handle-action-event state name key-pressed? tpf)
  ;;; TODO: implement event handling
  #!void)

(define-simple-class TransitionState (FabricClientState)
  ;; slots
  ;; methods
  ((cleanup) (%transition-state-cleanup (this)))
  ((initialize state-manager::AppStateManager app::FabricClient)
   (begin (invoke-special FabricClientState (this) 'initialize state-manager app)
          (set! app:state (this))
          (%transition-state-initialize (this))))
  ((isEnabled) (%transition-state-enabled? (this)))
  ((isInitialized) (%transition-state-initialized? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%transition-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%transition-state-detached (this) state-manager))
  ((handleAnalogEvent name value tpf)
   (%transition-state-handle-analog-event (this) name value tpf))
  ((handleActionEvent name key-pressed? tpf)
   (%transition-state-handle-action-event (this) name key-pressed? tpf)))

