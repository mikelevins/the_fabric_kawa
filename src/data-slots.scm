;;;; ***********************************************************************
;;;; Name:          data-slots.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       a convenient slot store for Frame classes
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FrameSlot slot-name set-slot-name!
               slot-value set-slot-value!
               slot-description set-slot-description!
               slot-getter set-slot-getter!
               slot-setter set-slot-setter!)

(require "utilities-java.scm")

(define-private-alias Keyword gnu.expr.Keyword)
(define-private-alias String java.lang.String)

(define-simple-class FrameSlot ()
  (name)
  ((getSlotName) name)
  ((setSlotName nm) (set! name nm))

  (value)
  ((getSlotValue)(if (absent? getter)
                     value
                     (getter name)))
  ((setSlotValue val) (if (absent? setter)
                          (set! value val)
                          (setter name val)))

  (description)
  ((getSlotDescription) description)
  ((setSlotDescription desc) (set! description desc))

  (getter)
  ((getSlotGetter) getter)
  ((setSlotGetter gt) (set! getter gt))

  (setter)
  ((getSlotSetter) setter)
  ((setSlotSetter st) (set! setter st)))

(define (slot-name slot::FrameSlot)
  (*:getSlotName slot))

(define (set-slot-name! slot::FrameSlot nm::Keyword)
  (*:setSlotName slot nm))

(define (slot-value slot::FrameSlot)
  (*:getSlotValue slot))

(define (set-slot-value! slot::FrameSlot val)
  (*:setSlotValue slot val))

(define (slot-description slot::FrameSlot)
  (*:getSlotDescription slot))

(define (set-slot-description! slot::FrameSlot desc::String)
  (*:setSlotDescription slot desc))

(define (slot-getter slot::FrameSlot)
  (*:getSlotGetter slot))

(define (set-slot-getter! slot::FrameSlot getter)
  (*:setSlotGetter slot getter))

(define (slot-setter slot::FrameSlot)
  (*:getSlotSetter slot))

(define (set-slot-setter! slot::FrameSlot setter)
  (*:setSlotSetter slot setter))
