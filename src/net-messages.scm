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

(module-export ChatMessage)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "utilities-java.scm")
(require "interface-frame.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias AbstractMessage com.jme3.network.AbstractMessage)
(define-private-alias Serializable com.jme3.network.serializing.Serializable)
(define-private-alias String java.lang.String)

;;; ---------------------------------------------------------------------
;;; ChatMessage
;;; ---------------------------------------------------------------------

(define-simple-class ChatMessage (AbstractMessage IMutableFrame)(@Serializable)
  ;; slots
  ;; ---------
  (name :: String init-form: #!null)
  (contents :: String init-form: #!null)

  ;; methods
  ;; ---------
  ((toString) (format #f "[~A] ~A" name contents))

  ;; Frame APIs
  ;; ---------
  ((frameKeys) (list name: contents: reliable:))
  ((containsFrameKey key) (member key (*:frameKeys (this))))
  ((getFrameKey key)(case key
                      ((name:) name)
                      ((contents:) contents)
                      ((reliable:) (*:isReliable (this)))
                      (else (error (format #f "Unrecognized ChatMessage key: ~S" key)))))
  ((setFrameKey key val)(case key
                          ((name:) (set! name val))
                          ((contents:) (set! contents val))
                          ((reliable:) (*:setReliable (this) val))
                          (else (error (format #f "Unrecognized ChatMessage key: ~S" key)))))
  ((deleteFrameKey key) (error "Cannot delete slots from a Message!")))



