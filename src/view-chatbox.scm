;;;; ***********************************************************************
;;;;
;;;; Name:          view-chatbox.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the chat box 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-chatbox)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")
(require "client-main.scm")
(require "model-rect.scm")
(require "view-actionbar.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ChatBox tonegod.gui.controls.extras.ChatBox)
(import-as Screen tonegod.gui.core.Screen)
(import-as TextField tonegod.gui.controls.text.TextField)
(import-as Vector2f com.jme3.math.Vector2f)

;;; ---------------------------------------------------------------------
;;; the chatbox class
;;; ---------------------------------------------------------------------

(define (compute-chatbox-rect screen::Screen)
  (let* ((action-bar-rect (compute-action-bar-rect screen))
         (screen-height (*:getHeight screen))
         (chatbox-width (- (get-left action-bar-rect) 32))
         (chatbox-height 256)
         (chatbox-left 16)
         (chatbox-top (- screen-height (+ chatbox-height 16))))
    (make-rectangle chatbox-left
                    chatbox-top
                    chatbox-width
                    chatbox-height)))

(defclass FabricChat (ChatBox)
  (slots: (chatname type: String init-form: "" getter: getChatName setter: setChatName))
  (methods:
   ((*init* screen :: Screen id :: String position :: Vector2f size :: Vector2f)
    (invoke-special ChatBox (this) '*init* screen id position size))
   ((onSendMsg msg :: String)
    (let* ((chatfield::TextField (*:getChildElementById (this) "chatbox:ChatInput"))
           (screen (*:getScreen (this)))
           (app (*:getApplication screen))
           ;;(chat-message (ChatMessage))
           )
      ;;(*:setName chat-message chatname)
      ;;(*:setContents chat-message msg)
      ;;(*:setReliable chat-message #t)
      ;;(send-chat-message app chat-message)
      (*:resetTabFocus chatfield)))))

(define (make-chatbox screen::Screen)
  (let ((rect (compute-chatbox-rect screen)))
    (FabricChat screen "ChatBox"
                (Vector2f (get-left rect)(get-top rect))
                (Vector2f (get-width rect)(get-height rect)))))
