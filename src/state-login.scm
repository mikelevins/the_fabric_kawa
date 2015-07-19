;;;; ***********************************************************************
;;;;
;;;; Name:          client-state-login.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading client-state-login.scm")

(module-export
 LoginState
 make-login-state)

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
;;; the client login AppState class
;;; ---------------------------------------------------------------------

;;; CLASS LoginState
;;; ---------------------------------------------------------------------

(define-simple-class LoginState (FabricClientState)
  ;; slots
  (initialized? init: #f)
  ((getInitialized) initialized?)
  ((setInitialized newstate) (set! initialized? newstate))
  (login-box init: #!null)
  ((getLoginBox) login-box)
  ((setLoginBox newbox) (set! login-box newbox))
  ;; methods
  ((cleanup) (%login-state-cleanup (this)))
  ((isEnabled) (%login-state-enabled? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%login-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%login-state-detached (this) state-manager))  
  ;; init
  ((initialize) (%login-state-initialize (this)))
  ((isInitialized) (%login-state-initialized? (this))))

(define (%login-state-cleanup state::LoginState)
  (format #t "~%%login-state-cleanup called"))

(define (%login-state-initialize state::LoginState)
  (format #t "~%%login-state-initialize called"))

(define (%login-state-enabled? state::LoginState) #t)

(define (%login-state-initialized? state::LoginState) #t)

(define (%login-state-attached state::LoginState manager::AppStateManager)
  (format #t "~%%login-state-attached called"))

(define (%login-state-detached state::LoginState manager::AppStateManager)
  (format #t "~%%login-state-detached called"))

(define (make-login-state client::Application)
  (let ((state (LoginState)))
    (*:setClient state client)
    state))
