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

(module-export <chat-message>)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------



;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias AbstractMessage com.jme3.network.AbstractMessage)
(define-private-alias String java.lang.String)

;;; ---------------------------------------------------------------------
;;; <chat-message>
;;; ---------------------------------------------------------------------

(define-simple-class <chat-message> (AbstractMessage)
  (name :: String init-form: #!null)
  (message :: String init-form: #!null)

  ((getName) name)
  ((setName nm) (set! name nm))
  ((getMessage) message)
  ((setMessage msg)(set! message msg))
  ((toString) (format #f "~A:~A" name message)))

