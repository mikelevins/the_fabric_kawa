;;;; ***********************************************************************
;;;; Name:          ui-client-chat.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       status and heads-up displays
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export init-client-chat)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "net-messages.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias AppSettings com.jme3.system.AppSettings)
(define-private-alias BitmapFont com.jme3.font.BitmapFont)
(define-private-alias ChatBox tonegod.gui.controls.extras.ChatBox)
(define-private-alias ColorRGBA com.jme3.math.ColorRGBA)
(define-private-alias EffectEvent tonegod.gui.effects.Effect:EffectEvent)
(define-private-alias KeyInput com.jme3.input.KeyInput)
(define-private-alias Screen tonegod.gui.core.Screen)
(define-private-alias String java.lang.String)
(define-private-alias TTextField tonegod.gui.controls.text.TextField)
(define-private-alias Vector2f com.jme3.math.Vector2f)

;;; ---------------------------------------------------------------------
;;; class FabricChat
;;; ---------------------------------------------------------------------
;;; a chatbox class for The Fabric

(define-simple-class FabricChat (ChatBox)
  ((*init* screen :: Screen id :: String position :: Vector2f size :: Vector2f)
   (invoke-special ChatBox (this) '*init* screen id position size))
  ((onSendMsg msg::String) (let* ((chatfield::TTextField (*:getChildElementById (this) "chatbox:ChatInput"))
                                  (screen::Screen (*:getScreen (this)))
                                  (app (*:getApplication screen))
                                  (player (get-key app player:))
                                  (chat-message::ChatMessage (ChatMessage)))
                             (set-key! chat-message name: (get-key player name:))
                             (set-key! chat-message contents: msg)
                             (set-key! chat-message reliable: #t)
                             (set-key! app chat-message: chat-message)
                             (*:resetTabFocus chatfield))))


;;; ---------------------------------------------------------------------
;;; set up the chat UI
;;; ---------------------------------------------------------------------

(define (init-client-chat app)
  (let* ((screen::Screen (get-key app gui-screen:))
         (key-input ::KeyInput (get-key app key-input:))
         (settings::AppSettings (get-key app settings:))
         (Align BitmapFont:Align)
         (VAlign BitmapFont:VAlign)
         (width (*:getWidth settings))
         (height (*:getHeight settings))
         (chatbox (FabricChat screen "chatbox"
                              (Vector2f 15 (- height 220))
                              (Vector2f 400 200)))
         (chatfield (*:getChildElementById chatbox "chatbox:ChatInput"))
         (chatlog (*:getChildElementById chatbox "chatbox:ChatArea")))

    (*:setFontColor chatlog ColorRGBA:Green)
    (*:removeEffect chatfield EffectEvent:TabFocus)
    (*:setFontSize chatfield 28)
    (*:setSendKey chatbox key-input:KEY_RETURN)
    
    (*:addElement screen chatbox)))

