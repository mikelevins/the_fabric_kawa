;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-login.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       log in to the Fabric server
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export LoginAppState)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")
(require "net-messaging.scm")

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

(defclass FabricLoginBox (LoginBox)
  (methods:
   ((*init* screen :: Screen uid :: String position :: Vector2f size :: Vector2f)
    (invoke-special LoginBox (this) '*init* screen uid position size))
   ((onButtonLoginPressed evt::MouseButtonEvent toggle::boolean) #!void)
   ((onButtonCancelPressed evt::MouseButtonEvent toggle::boolean) #!void)))


(defclass LoginAppState (AbstractAppState)
  (slots:
   (app::SimpleApplication init-form: #!null getter: getApp setter: setApp)
   (state-manager::AppStateManager init-form: #!null getter: getStateManager setter: setStateManager)
   (network-client::com.jme3.network.Client
    init-form: #!null getter: getNetworkClient setter: setNetworkClient)
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
