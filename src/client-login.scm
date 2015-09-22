;;;; ***********************************************************************
;;;;
;;;; Name:          client-login.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       The Fabric login UI
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 LoginClient
 activate-login
 password-hash
 username)

;;; ---------------------------------------------------------------------
;;; require modules
;;; ---------------------------------------------------------------------

(require util-crypt)
(require model-rect)
(require model-namegen)
(require model-user)
(require view-loginbox)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app SimpleApplication))
(import (class com.jme3.math Vector2f))
(import (class com.jme3.scene Node))
(import (class com.jme3.system AppSettings))
(import (class org.lwjgl.input Mouse))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; LoginClient
;;; ---------------------------------------------------------------------

(define username (make-parameter #f))
(define password-hash (make-parameter #f))

(define-simple-class LoginClient (SimpleApplication)
  ;; slots
  (settings init: #!null)
  (state init: #!null)
  (screen init: #!null)
  ;; init
  ((simpleInitApp) (init-login-client (this))))

(define (init-login-client client::LoginClient)
  (let* ((screen (Screen client))
         (gui-node::Node (*:getGuiNode client))
         (root::Node (*:getRootNode client))
         (rect (compute-login-box-rect screen))
         (box::FabricLoginBox
          (FabricLoginBox screen "LoginBox"
                          (Vector2f (get-left rect) (get-top rect))
                          (Vector2f (get-width rect) (get-height rect)))))
    (set! client:screen screen)
    (*:setEnabled (*:getFlyByCamera client) #f)
    (*:setWindowTitle box "Log in to the Fabric")
    (*:addElement screen box)
    (*:addControl gui-node screen)
    #!void))

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

(define (activate-login)
  (let* ((client::LoginClient (LoginClient))
         (settings::AppSettings (AppSettings #t)))
    (*:setSettings client settings)
    (*:setResolution settings 800 600)
    (*:setTitle settings "Fabric Login")
    (*:setDisplayFps client #f)
    (*:setShowSettings client #f)
    (*:setDisplayStatView client #f)
    (*:setPauseOnLostFocus client #t)
    (Mouse:setGrabbed #f)
    (*:start client)))
