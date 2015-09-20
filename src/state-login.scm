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

(require util-error)
(require util-java)
(require util-lists)
(require data-nodes)
(require client-state)
(require client-class)
(require model-rect)
(require view-loginbox)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app Application))
(import (class com.jme3.app.state AppStateManager))
(import (class com.jme3.math Vector2f))
(import (class com.jme3.scene Node Spatial))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; the client login AppState class
;;; ---------------------------------------------------------------------

;;; CLASS LoginState
;;; ---------------------------------------------------------------------

(define-simple-class LoginState (FabricClientState)
  ;; slots
  (initialized? init: #f)
  (login-box init: #!null)
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
  (let ((client::Application state:client))
    (prepare-to-attach-login-client-state state client)
    (did-attach-login-client-state state manager)))

(define (%login-state-detached state::LoginState manager::AppStateManager)
  (did-detach-login-client-state state manager))

(define (make-login-state client::Application)
  (let ((state (LoginState)))
    (set! state:client client)
    state))

;;; ---------------------------------------------------------------------
;;; LoginState functions
;;; ---------------------------------------------------------------------

(define (compute-login-box-rect screen::Screen)
  (let* ((screen-width screen:width)
         (screen-height screen:height)
         (box-width 700)
         (box-height 300)
         (box-left (- (/ screen-width 2)
                      (/ box-width 2)))
         (box-top (- (/ screen-height 2)
                     (/ box-height 2))))
    (make-rectangle box-left
                    box-top
                    box-width
                    box-height)))

(define (prepare-to-attach-login-client-state state::LoginState client::FabricClient)
  (unless state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode)
           (rect (compute-login-box-rect screen))
           (box::FabricLoginBox (FabricLoginBox screen "LoginBox"
                                                (Vector2f (get-left rect) (get-top rect))
                                                (Vector2f (get-width rect) (get-height rect)))))
      (*:setWindowTitle box "Log in to the Fabric")
      (set! state:login-box box)
      (set! state:initialized? #t))))

(define (did-attach-login-client-state state::LoginState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode))
      (*:enqueue client
                 (runnable (lambda ()
                             (let* ((client::FabricClient state:client)
                                    (root::Node client:rootNode))
                               (*:addElement screen state:login-box)
                               (*:addControl gui-node screen))))))))

(define (did-detach-login-client-state state::LoginState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode))
      (*:enqueue client
                 (runnable (lambda ()
                             (let* ((client::FabricClient state:client)
                                    (root::Node client:rootNode))
                               (*:removeElement screen state:login-box)
                               (*:removeControl gui-node screen)
                               (set! state:initialized? #f))))))))
