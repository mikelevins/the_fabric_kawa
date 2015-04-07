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
(require "syntax-classes.scm")
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

(defclass LoginState (FabricClientState)
  (slots:
   (initialized? init-form: #f getter: getInitialized setter: setInitialized)
   (login-box init-form: #!null getter: getLoginBox setter: setLoginBox)) 
  (methods:
   ((cleanup) (%login-state-cleanup (this)))
   ((initialize) (%login-state-initialize (this)))
   ((isEnabled) (%login-state-enabled? (this)))
   ((isInitialized) (%login-state-initialized? (this)))
   ((stateAttached state-manager::AppStateManager)
    (%login-state-attached (this) state-manager))
   ((stateDetached state-manager::AppStateManager)
    (%login-state-detached (this) state-manager))))

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
