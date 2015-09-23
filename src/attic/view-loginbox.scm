;;;; ***********************************************************************
;;;;
;;;; Name:          view-loginbox.scm
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

(require util-error)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.input.event MouseButtonEvent))
(import (class com.jme3.math Vector2f))
(import (class tonegod.gui.controls.windows LoginBox))
(import (class tonegod.gui.core Screen))

;;; CLASS FabricLoginBox
;;; ---------------------------------------------------------------------
;;; a LoginBox subclass that presents a form that enables players to
;;; log in to the remote Fabric server in order to play

(define-simple-class FabricLoginBox (LoginBox)
  ((*init* screen::Screen uid::String position::Vector2f size::Vector2f)
   (invoke-special LoginBox (this) '*init* screen uid position size))
  ((onButtonLoginPressed evt::MouseButtonEvent toggle::boolean)
   (let ((server-connection #!null #|(connect-to-server)|#))
     #|(if (eqv? #!null server-connection)
         (warn "Connection to server failed")
         (let ((client::FabricClient app))
           (warn "Connection to server succeeded")
           (*:setNetworkClient client server-connection)
           ))|#
     #f))
  ((onButtonCancelPressed evt::MouseButtonEvent toggle::boolean)
   (*:stop app)))
