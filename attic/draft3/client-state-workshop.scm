;;;; ***********************************************************************
;;;;
;;;; Name:          client-state-workshop.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       supporting functions for WorkshopClientState
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 did-attach-workshop-client-state
 did-detach-workshop-client-state
 prepare-to-attach-workshop-client-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "util-lists.scm")
(require "syntax-classes.scm")
(require "view-loginbox.scm")
(require "client-states.scm")
(require "client-main.scm")
(require "view-skybox.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as Spatial com.jme3.scene.Spatial)
(import-as Vector2f com.jme3.math.Vector2f)


;;; ---------------------------------------------------------------------
;;; WorkshopClientState functions
;;; ---------------------------------------------------------------------

(define (prepare-to-attach-workshop-client-state state::WorkshopClientState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client))
           (sky::Spatial (make-sky-box))
           (Align BitmapFont:Align))
      (*:setSky state sky)
      (*:setInitialized state #t))))

(define (did-attach-workshop-client-state state::WorkshopClientState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((client::FabricClient (*:getApp state))
                                   (root::Node (*:getRootNode client)))
                               (*:attachChild root (*:getSky state))
                               (*:addControl gui-node screen))))))))

(define (did-detach-workshop-client-state state::WorkshopClientState mgr::AppStateManager)
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
                               (*:removeControl gui-node screen)
                               (*:setInitialized state #f))))))))
