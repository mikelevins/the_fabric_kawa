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
;;; 

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")
(require "net-messaging.scm")
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
(import-as LoginBox tonegod.gui.controls.windows.LoginBox)
(import-as MouseButtonEvent com.jme3.input.event.MouseButtonEvent)
(import-as Network com.jme3.network.Network)
(import-as Screen tonegod.gui.core.Screen)
(import-as Serializer com.jme3.network.serializing.Serializer)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector4f com.jme3.math.Vector4f)
(import-as Window tonegod.gui.controls.windows.Window)

;;; ---------------------------------------------------------------------
;;; the LoginAppState class
;;; ---------------------------------------------------------------------

;;; (connect-to-server)
;;; ---------------------------------------------------------------------
;;; TODO: make this function available to other AppStates
;;; attempts to open a network connection from the Fabric client to
;;; the server in order to pass messages

(define (connect-to-server)
  (try-catch
   (let ((new-connection (Network:connectToServer (server-name)(server-version)(server-host)
                                                  (server-port)(server-port))))
     (*:start new-connection)
     new-connection)
   (ex ConnectException (begin (warn "failed to connect to Fabric server.")
                               (warn "~A" (*:toString ex))
                               #!null))))

;;; CLASS FabricLoginBox
;;; ---------------------------------------------------------------------
;;; a LoginBox subclass that presents a form that enables players to
;;; log in to the remote Fabric server in order to play

(defclass FabricLoginBox (LoginBox)
  (methods:
   ((*init* screen::Screen uid::String position::Vector2f size::Vector2f)
    (invoke-special LoginBox (this) '*init* screen uid position size))
   ((onButtonLoginPressed evt::MouseButtonEvent toggle::boolean)
    (let ((server-connection (connect-to-server)))
      (if (jnull? server-connection)
          (warn "Connection to server failed")
          (let ((client::FabricClient app))
            (warn "Connection to server succeeded")
            (*:setNetworkClient client server-connection)))))
   ((onButtonCancelPressed evt::MouseButtonEvent toggle::boolean)
    (*:stop app))))

;;; CLASS LoginAppState
;;; ---------------------------------------------------------------------
;;; an AppState class that constructs and managers the Login scene in
;;; the Fabric client

(defclass LoginAppState (AbstractAppState)
  (slots:
   (app::SimpleApplication init-form: #!null getter: getApp setter: setApp)
   (state-manager::AppStateManager init-form: #!null getter: getStateManager setter: setStateManager)
   (login-box::FabricLoginBox init-form: #!null getter: getLoginBox setter: setLoginBox))
  (methods:
   ((initialize mgr::AppStateManager client::SimpleApplication)
    (*:setApp (this) client)
    (let* ((screen (Screen client))
           (gui-node (*:getGuiNode client))
           (win (FabricLoginBox screen "login" (Vector2f 700 300)(Vector2f 700 300))))
      (*:setLoginBox (this) win)
      (*:addElement screen win)
      (*:addControl gui-node screen)))))
