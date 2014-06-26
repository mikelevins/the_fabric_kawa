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

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias AbstractMessage com.jme3.network.AbstractMessage)
(define-private-alias String java.lang.String)

;;; ---------------------------------------------------------------------
;;; ChatMessage
;;; ---------------------------------------------------------------------

(define-simple-class ChatMessage (AbstractMessage)
  (name :: String init-form: #!null)
  (contents :: String init-form: #!null)

  ((getName) name)
  ((setName nm) (set! name nm))
  ((getContents) contents)
  ((setContents msg)(set! contents msg))
  ((toString) (format #f "~A:~A" name contents)))

(defgetter (message-name ChatMessage) getName)
(defgetter (message-contents ChatMessage) getContents)
(defgetter (message-reliable? ChatMessage) isReliable)

(defsetter (set-message-name! ChatMessage) setName)
(defsetter (set-message-contents! ChatMessage) setContents)
(defsetter (set-message-reliable! ChatMessage) setReliable)


