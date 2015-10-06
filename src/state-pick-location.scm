;;;; ***********************************************************************
;;;;
;;;; Name:          state-pick-location.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the location picker
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 PickLocationState
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
;;; PickLocationState
;;; ---------------------------------------------------------------------

(define (%pick-location-state-cleanup state::FabricClientState)
  (format #t "~%PickLocationState cleaned up"))

(define (%pick-location-state-initialize state::FabricClientState)
  (format #t "~%PickLocationState initialized")
  (set! state:state-initialized? #t))

(define (%pick-location-state-enabled? state::FabricClientState) #t)
(define (%pick-location-state-initialized? state::FabricClientState) state:state-initialized?)
(define (%pick-location-state-attached state::FabricClientState manager::AppStateManager) #!void)
(define (%pick-location-state-detached state::FabricClientState manager::AppStateManager) #!void)

(define (%pick-location-state-handle-analog-event state name value tpf)
  ;;; TODO: implement event handling
  #!void)

(define (%pick-location-state-handle-action-event state name key-pressed? tpf)
  ;;; TODO: implement event handling
  #!void)

(define-simple-class PickLocationState (FabricClientState)
  ;; slots
  (user::FabricUser init: #!null)
  (character::FabricCharacter init: #!null)
  ;; methods
  ((*init* a-user::FabricUser a-character::FabricCharacter)
   (set! user a-user)
   (set! character a-character))
  ((cleanup) (%pick-location-state-cleanup (this)))
  ((initialize state-manager::AppStateManager app::FabricClient)
   (begin (invoke-special FabricClientState (this) 'initialize state-manager app)
          (set! app:state (this))
          (%pick-location-state-initialize (this))))
  ((isEnabled) (%pick-location-state-enabled? (this)))
  ((isInitialized) (%pick-location-state-initialized? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%pick-location-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%pick-location-state-detached (this) state-manager))
  ((handleAnalogEvent name value tpf)
   (%pick-location-state-handle-analog-event (this) name value tpf))
  ((handleActionEvent name key-pressed? tpf)
   (%pick-location-state-handle-action-event (this) name key-pressed? tpf)))

