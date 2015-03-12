;;;; ***********************************************************************
;;;;
;;;; Name:          gamestates-transit.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       supporting functions for TransitGameState
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 did-attach-transit-gamestate
 did-detach-transit-gamestate
 prepare-to-attach-transit-gamestate)

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
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)


;;; ---------------------------------------------------------------------
;;; TransitGameState functions
;;; ---------------------------------------------------------------------

(define (prepare-to-attach-transit-gamestate state::TransitGameState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client))
           (Align BitmapFont:Align)
           (label::Label (Label screen "FactionNameplate" (Vector2f 600 40)(Vector2f 1200 40))))
      (*:setFactionNameplate state label)
      (*:setText label "In transit...")
      (*:setTextAlign label Align:Left)
      (*:setFont label "Interface/Fonts/Laconic30.fnt")
      (*:setFontSize label 30)
      (*:setFontColor label ColorRGBA:Green)
      (*:setInitialized state #t))))

(define (did-attach-transit-gamestate state::TransitGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:addElement screen (*:getStatusLabel state))
                             (*:addControl gui-node screen)))))))

(define (did-detach-transit-gamestate state::TransitGameState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getApp state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:removeElement screen (*:getStatusLabel state))
                             (*:removeControl gui-node screen)))))))
