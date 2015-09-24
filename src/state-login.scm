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
(require view-loginbox)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app.state AbstractAppState AppStateManager))
(import (class com.jme3.scene Node))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; LoginState
;;; ---------------------------------------------------------------------

(define-simple-class LoginState (FabricClientState)
  ;; slots
  (loginbox::FabricLoginBox init: #!null)
  ;; methods
  ((cleanup) (%login-state-cleanup (this)))
  ((initialize state-manager::AppStateManager app::FabricClient)
   (begin (invoke-special FabricClientState (this) 'initialize state-manager app)
          (set! app:state (this))
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

(define (%login-state-cleanup state::LoginState)
  (let* ((client::FabricClient state:client)
         (screen::Screen client:screen)
         (gui-node::Node (*:getGuiNode client)))
    (*:removeElement screen state:loginbox)
    (*:removeControl gui-node screen)))

(define (%login-state-initialize state::LoginState)
  (let* ((client::FabricClient state:client)
         (screen::Screen client:screen)
         (loginbox::FabricLoginBox (make-loginbox state))
         (gui-node::Node (*:getGuiNode client)))
    (set! state:loginbox loginbox)
    (*:addElement screen loginbox)
    (*:addControl gui-node screen)
    (set! state:state-initialized? #t)))

(define (%login-state-enabled? state::LoginState) #t)
(define (%login-state-initialized? state::LoginState) state:state-initialized?)
(define (%login-state-attached state::LoginState manager::AppStateManager) #!void)
(define (%login-state-detached state::LoginState manager::AppStateManager) #!void)

(define (%login-state-handle-analog-event state name value tpf)
  ;;; TODO: implement event handling
  #!void)

(define (%login-state-handle-action-event state name key-pressed? tpf)
  ;;; TODO: implement event handling
  #!void)
