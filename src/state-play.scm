;;;; ***********************************************************************
;;;;
;;;; Name:          state-play.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the play AppState
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 PlayState
 )

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require data-assets)
(require state)
(require client)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app.state AbstractAppState AppStateManager))

;;; ---------------------------------------------------------------------
;;; PlayState
;;; ---------------------------------------------------------------------

(define (%play-state-cleanup state::FabricClientState)
  (format #t "~%PlayState cleaned up"))

(define (%play-state-initialize state::FabricClientState)
  (format #t "~%PlayState initialized")
  (set! state:state-initialized? #t))

(define (%play-state-enabled? state::FabricClientState) #t)
(define (%play-state-initialized? state::FabricClientState) state:state-initialized?)
(define (%play-state-attached state::FabricClientState manager::AppStateManager) #!void)
(define (%play-state-detached state::FabricClientState manager::AppStateManager) #!void)
(define (%play-state-handle-analog-event state name value tpf)
  (warn "%play-state-handle-analog-event is not yet implemented"))
(define (%play-state-handle-action-event state name key-pressed? tpf)
  (warn "%play-state-handle-action-event is not yet implemented"))

(define-simple-class PlayState (FabricClientState)
  ;; slots
  ;; methods
  ((cleanup) (%play-state-cleanup (this)))
  ((initialize state-manager::AppStateManager app::FabricClient)
   (begin (invoke-special FabricClientState (this) 'initialize state-manager app)
          (set! app:state (this))
          (%play-state-initialize (this))))
  ((isEnabled) (%play-state-enabled? (this)))
  ((isInitialized) (%play-state-initialized? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%play-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%play-state-detached (this) state-manager))
  ((handleAnalogEvent name value tpf)
   (%play-state-handle-analog-event (this) name value tpf))
  ((handleActionEvent name key-pressed? tpf)
   (%play-state-handle-action-event (this) name key-pressed? tpf)))


