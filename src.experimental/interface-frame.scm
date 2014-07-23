;;;; ***********************************************************************
;;;; Name:          interface-frame.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       interfaces for frame-like objects
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 IFrame keys contains-key? get-key set-key! 
 get-slot-getter set-slot-getter! 
 get-slot-setter set-slot-setter!)

(require "utilities-java.scm")

;;; ---------------------------------------------------------------------
;;; IFrame - common frame API
;;; ---------------------------------------------------------------------

(define-simple-class IFrame () interface: #t
  ((frameKeys) #!abstract)
  ((containsFrameKey key) #!abstract)
  ((getFrameKey key) #!abstract)
  ((setFrameKey key val) #!abstract)
  ((getSlotDescription key) #!abstract)
  ((setSlotDescription key description) #!abstract)
  ((getSlotGetter key) #!abstract)
  ((setSlotGetter key getter) #!abstract)
  ((getSlotSetter key) #!abstract)
  ((setSlotSetter key setter) #!abstract))

(define (keys fr::IFrame)
  (*:frameKeys fr))

(define (contains-key? fr::IFrame key)
  (*:containsFrameKey fr key))

(define (get-key fr::IFrame key #!optional (default #f))
  (let ((val (*:getFrameKey fr key)))
    (if (jnull? val)
        default
        val)))

(define (set-key! fr::IFrame key val)
  (*:setFrameKey fr key val))

(define (get-slot-description fr::IFrame key)
  (*:getSlotDescription fr key))

(define (set-slot-description! fr::IFrame key getter)
  (*:setSlotDescription fr key description))

(define (get-slot-getter fr::IFrame key)
  (*:getSlotGetter fr key))

(define (set-slot-getter! fr::IFrame key getter)
  (*:setSlotGetter fr key getter))

(define (get-slot-setter fr::IFrame key)
  (*:getSlotSetter fr key))

(define (set-slot-setter! fr::IFrame key setter)
  (*:setSlotSetter fr key setter))
