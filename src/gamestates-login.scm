;;;; ***********************************************************************
;;;;
;;;; Name:          gamestates-login.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       supporting functions for LoginGameState
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 did-attach-login-gamestate
 did-detach-login-gamestate
 prepare-to-attach-login-gamestate)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "util-lists.scm")
(require "syntax-classes.scm")
(require "view-loginbox.scm")
(require "gamestates.scm")
(require "client-main.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)


;;; ---------------------------------------------------------------------
;;; LoginGameState functions
;;; ---------------------------------------------------------------------

(define (prepare-to-attach-login-gamestate state::LoginGameState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client))
           (screen-width (*:getWidth screen))
           (screen-height (*:getHeight screen))
           (box-width 700)
           (box-height 300)
           (box-left (- (/ screen-width 2) (/ box-width 2)))
           (box-top (- (/ screen-height 2) (/ box-height 2)))
           (box::FabricLoginBox (FabricLoginBox screen "LoginBox"
                                                (Vector2f box-left box-top)
                                                (Vector2f box-width box-height))))
      (*:setWindowTitle box "Log in to the Fabric")
      (*:setLoginBox state box)
      (*:setInitialized state #t))))

(define (did-attach-login-gamestate state::LoginGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:addElement screen (*:getLoginBox state))
                             (*:addControl gui-node screen)))))))

(define (did-detach-login-gamestate state::LoginGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:removeElement screen (*:getLoginBox state))
                             (*:removeControl gui-node screen)))))))
