;;;; ***********************************************************************
;;;;
;;;; Name:          client-state-transit.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       supporting functions for TransitClientState
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 did-attach-transit-client-state
 did-detach-transit-client-state
 prepare-to-attach-transit-client-state)

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
;;; TransitClientState functions
;;; ---------------------------------------------------------------------

(define (prepare-to-attach-transit-client-state state::TransitClientState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client))
           (sky::Spatial (make-sky-box))
           (Align BitmapFont:Align)
           (label::Label (Label screen "FactionNameplate" (Vector2f 600 40)(Vector2f 1200 40))))
      (*:setFactionNameplate state label)
      (*:setSky state sky)
      (*:setText label "In transit...")
      (*:setTextAlign label Align:Left)
      (*:setFont label "Interface/Fonts/Laconic30.fnt")
      (*:setFontSize label 30)
      (*:setFontColor label ColorRGBA:Green)
      (*:setInitialized state #t))))

(define (did-attach-transit-client-state state::TransitClientState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((client::FabricClient (*:getApp state))
                                   (root::Node (*:getRootNode client)))
                               (*:attachChild root (*:getSky state))
                               (*:addElement screen (*:getStatusLabel state))
                               (*:addControl gui-node screen))))))))

(define (did-detach-transit-client-state state::TransitClientState mgr::AppStateManager)
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
                               (*:removeElement screen (*:getStatusLabel state))
                               (*:removeControl gui-node screen)
                               (*:setInitialized state #f))))))))
