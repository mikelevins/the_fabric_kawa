;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-login.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       log in to the Fabric server
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 LoginAppState)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")
(require "init-config-local.scm")
(require "net-connect.scm")
(require "net-messaging.scm")
(require "view-login.scm")
(require "client-main.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as ButtonAdapter tonegod.gui.controls.buttons.ButtonAdapter)
(import-as Client com.jme3.network.Client)
(import-as ConnectException java.net.ConnectException)
(import-as DefaultClient com.jme3.network.base.DefaultClient)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Network com.jme3.network.Network)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as Serializer com.jme3.network.serializing.Serializer)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector4f com.jme3.math.Vector4f)
(import-as Window tonegod.gui.controls.windows.Window)

;;; ---------------------------------------------------------------------
;;; the LoginAppState class
;;; ---------------------------------------------------------------------

;;; CLASS LoginAppState
;;; ---------------------------------------------------------------------
;;; an AppState class that constructs and managers the Login scene in
;;; the Fabric client

(defclass LoginAppState (AbstractAppState)
  (slots:
   (app::SimpleApplication init-form: #!null getter: getApp setter: setApp)
   (state-manager::AppStateManager init-form: #!null getter: getStateManager setter: setStateManager)
   (initialized init-form: #f getter: getInitialized setter: setInitialized)
   (enabled init-form: #f getter: getEnabled setter: setEnabled)
   (login-box::FabricLoginBox init-form: #!null getter: getLoginBox setter: setLoginBox))
  (methods:
   ((initialize mgr::AppStateManager client::FabricClient)
    (invoke-special AbstractAppState (this) 'initialize mgr client)
    (init-login-state (this) mgr client))
   ((cleanup) #!void)
   ((isEnabled) enabled)
   ((isInitialized) initialized)
   ((stateAttached mgr::AppStateManager)(handle-state-attached (this) mgr))
   ((stateDetached mgr::AppStateManager)(handle-state-detached (this) mgr))))

(define (init-login-state state::LoginAppState mgr::AppStateManager client::FabricClient)
  (*:setApp state client)
  (*:setStateManager state mgr)
  (*:setInitialized state #t))

(define (handle-state-attached state::LoginAppState mgr::AppStateManager)
  (let ((client::FabricClient (*:getApplication mgr)))
    (if (not (*:getInitialized state))
        (*:initialize state mgr client))
    (let* ((screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client))
           (win::Window (FabricLoginBox screen
                                        (format #f "login~d" (next-session-id))
                                        (Vector2f 700 300)(Vector2f 700 300))))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:setLoginBox state win)
                             (*:addElement screen win)
                             (*:addControl gui-node screen)))))))


(define (handle-state-detached state::LoginAppState mgr::AppStateManager)
  (let* ((client::FabricClient (*:getApp state))
         (screen::Screen (*:getScreen client))
         (gui-node::Node (*:getGuiNode client))
         (win::Window (*:getLoginBox state)))
    (*:enqueue client
               (runnable (lambda ()
                           (*:removeElement screen win)
                           (*:removeControl gui-node screen))))))



