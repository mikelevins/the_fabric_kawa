;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          net-messaging.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       messages passed between client and server
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export ChatMessage message-name message-contents message-reliable?
               set-message-name! set-message-contents! set-message-reliable!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractMessage com.jme3.network.AbstractMessage)
(import-as Serializable com.jme3.network.serializing.Serializable)
(import-as String java.lang.String)

;;; ---------------------------------------------------------------------
;;; ChatMessage
;;; ---------------------------------------------------------------------

(defclass ChatMessage (AbstractMessage)
  (annotations: @Serializable)
  (slots:
   (name type: String init-form: #!null getter: getName setter: setName)
   (contents type: String init-form: #!null getter: getContents setter: setContents))
  (methods:
   ((toString) (format #f "[~A] ~A" name contents))))

(define (message-name msg::ChatMessage) (*:getName msg))
(define (message-contents msg::ChatMessage) (*:getContents msg))
(define (message-reliable? msg::ChatMessage) (*:isReliable msg))

(define (set-message-name! msg::ChatMessage nm) (*:setName msg nm))
(define (set-message-contents! msg::ChatMessage cts) (*:setContents msg cts))
(define (set-message-reliable! msg::ChatMessage rel?) (*:setReliable msg rel?))

