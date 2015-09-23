;;;; ***********************************************************************
;;;;
;;;; Name:          state-login.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the login AppState
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 LoginState
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
;;; LoginState
;;; ---------------------------------------------------------------------

(define (%login-state-cleanup state::FabricClientState) #!void)

(define (%login-state-initialize state::FabricClientState)
  (format #t "~%LoginState initialized")
  (set! state:state-initialized? #t))

(define (%login-state-enabled? state::FabricClientState) #t)
(define (%login-state-initialized? state::FabricClientState) state:state-initialized?)
(define (%login-state-attached state::FabricClientState manager::AppStateManager) #!void)
(define (%login-state-detached state::FabricClientState manager::AppStateManager) #!void)
(define (%login-state-handle-analog-event state name value tpf)
  (warn "%login-state-handle-analog-event is not yet implemented"))
(define (%login-state-handle-action-event state name key-pressed? tpf)
  (warn "%login-state-handle-action-event is not yet implemented"))

(define-simple-class LoginState (FabricClientState)
  ;; slots
  ;; methods
  ((cleanup) (%login-state-cleanup (this)))
  ((initialize state-manager::AppStateManager app::FabricClient)
   (begin (invoke-special FabricClientState (this) 'initialize state-manager app)
          (%login-state-initialize (this))))
  ((isEnabled) (%login-state-enabled? (this)))
  ((isInitialized) (%login-state-initialized? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%login-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%login-state-detached (this) state-manager))
  ((handleAnalogEvent name value tpf)
   (%login-state-handle-analog-event (this) name value tpf))
  ((handleActionEvent name key-pressed? tpf)
   (%login-state-handle-action-event (this) name key-pressed? tpf)))


