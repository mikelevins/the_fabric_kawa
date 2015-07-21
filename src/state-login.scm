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
(require "client-class.scm")
(require "view-loginbox.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Application com.jme3.app.Application)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as Spatial com.jme3.scene.Spatial)
(import-as Vector2f com.jme3.math.Vector2f)

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

;;; ---------------------------------------------------------------------
;;; LoginState functions
;;; ---------------------------------------------------------------------

(define (compute-login-box-rect screen::Screen)
  (let* ((screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
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
  (unless (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (sky::Spatial (make-sky-box))
           (gui-node::Node (*:getGuiNode client))
           (rect (compute-login-box-rect screen))
           (box::FabricLoginBox (FabricLoginBox screen "LoginBox"
                                                (Vector2f (get-left rect) (get-top rect))
                                                (Vector2f (get-width rect) (get-height rect)))))
      (*:setWindowTitle box "Log in to the Fabric")
      (*:setLoginBox state box)
      (*:setSky state sky)
      (*:setInitialized state #t))))

(define (did-attach-login-client-state state::LoginState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((client::FabricClient (*:getApp state))
                                   (root::Node (*:getRootNode client)))
                               (*:attachChild root (*:getSky state))
                               (*:addElement screen (*:getLoginBox state))
                               (*:addControl gui-node screen))))))))

(define (did-detach-login-client-state state::LoginState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((client::FabricClient (*:getApp state))
                                   (root::Node (*:getRootNode client))
                                   (sky::Spatial (*:getSky state)))
                               (*:detachChild root sky)
                               (*:setSky state #!null)
                               (*:removeElement screen (*:getLoginBox state))
                               (*:removeControl gui-node screen)
                               (*:setInitialized state #f))))))))
