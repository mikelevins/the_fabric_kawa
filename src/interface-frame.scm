;;;; ***********************************************************************
;;;; Name:          interface-frame.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       interfaces for frame-like objects
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 IFrame keys contains-key? get-key
 IPersistentFrame put-key remove-key
 IMutableFrame set-key! delete-key!
 ISlotAccessors set-slot-getter! delete-slot-getter! set-slot-setter! delete-slot-setter!)

(require "utilities-java.scm")

;;; ---------------------------------------------------------------------
;;; IFrame - common frame API
;;; ---------------------------------------------------------------------

(define-simple-class IFrame () interface: #t
  ((frameKeys) #!abstract)
  ((containsFrameKey key) #!abstract)
  ((getFrameKey key) #!abstract))

(define (keys fr::IFrame)
  (*:frameKeys fr))

(define (contains-key? fr::IFrame key)
  (*:containsFrameKey fr key))

(define (get-key fr::IFrame key #!optional (default #f))
  (let ((val (*:getFrameKey fr key)))
    (if (jnull? val)
        default
        val)))

;;; ---------------------------------------------------------------------
;;; IPersistentFrame - persistent, immutable frames
;;; ---------------------------------------------------------------------

(define-simple-class IPersistentFrame (IFrame) interface: #t
  ((putFrameKey key val) #!abstract)
  ((removeFrameKey key) #!abstract))

(define (put-key fr::IPersistentFrame key val)
  (*:putFrameKey fr key val))

(define (remove-key fr::IPersistentFrame key)
  (*:removeFrameKey fr key))

;;; ---------------------------------------------------------------------
;;; IMutableFrame - mutable frames
;;; ---------------------------------------------------------------------

(define-simple-class IMutableFrame (IFrame) interface: #t
  ((setFrameKey key val) #!abstract)
  ((deleteFrameKey key) #!abstract))

(define (set-key! fr::IMutableFrame key val)
  (*:setFrameKey fr key val))

(define (delete-key! fr::IMutableFrame key)
  (*:deleteFrameKey fr key))

;;; ---------------------------------------------------------------------
;;; ISlotAccessors - frames with getter and setter functions
;;; ---------------------------------------------------------------------

(define-simple-class ISlotAccessors (IMutableFrame) interface: #t
  ((getSlotGetter key) #!abstract)
  ((setSlotGetter key getter) #!abstract)
  ((deleteSlotGetter key) #!abstract)
  ((getSlotSetter key) #!abstract)
  ((setSlotSetter key setter) #!abstract)
  ((deleteSlotSetter key) #!abstract))

(define (get-slot-getter! fr::ISlotAccessors key)
  (*:getSlotGetter fr key))

(define (set-slot-getter! fr::ISlotAccessors key getter)
  (*:setSlotGetter fr key getter))

(define (delete-slot-getter! fr::ISlotAccessors key)
  (*:deleteSlotGetter fr key))

(define (set-slot-setter! fr::ISlotAccessors key setter)
  (*:setSlotSetter fr key setter))

(define (delete-slot-setter! fr::ISlotAccessors key)
  (*:deleteSlotSetter fr key))
