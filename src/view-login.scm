;;;; ***********************************************************************
;;;;
;;;; Name:          view-login.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the login dialog 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricLoginBox)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")
(require "net-connect.scm")
(require "client-main.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as LoginBox tonegod.gui.controls.windows.LoginBox)
(import-as MouseButtonEvent com.jme3.input.event.MouseButtonEvent)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)

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
