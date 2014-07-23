;;;; ***********************************************************************
;;;; Name:          player.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       representation of players
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-player init-player)

(require "data-pmaps.scm")
(require "interface-frame.scm")

(define-private-alias HashPMap org.pcollections.HashPMap)
(define-private-alias Node com.jme3.scene.Node)
(define-private-alias PI com.jme3.math.FastMath:PI)
(define-private-alias Quaternion com.jme3.math.Quaternion)
(define-private-alias Vector3f com.jme3.math.Vector3f)

(define-simple-class Player (IFrame)
  ;; slots
  ;; ---------
  (slots::HashPMap init-form: (hashpmap))
  ((getSlots) slots)
  ((setSlots new-slots) (set! slots new-slots))

  (slotGetters::HashPMap init-form: (hashpmap))
  ((getSlotGetter key) (*:get slotGetters key))
  ((setSlotGetter key getter) (set! slotGetters (*:plus slotGetters key getter)))
  ((deleteSlotGetter key) (set! slotGetters (*:minus slotGetters key)))

  (slotSetters::HashPMap init-form: (hashpmap))
  ((getSlotSetter key) (*:get slotSetters key))
  ((setSlotSetter key setter) (set! slotSetters (*:plus slotSetters key setter)))
  ((deleteSlotSetter key) (set! slotSetters (*:minus slotSetters key)))
  
  ;; IFrame, IMutableFrame, ISlotAccessors
  ;; ---------
  ((frameKeys) (map-keys slots))
  ((containsFrameKey key) (member key (*:frameKeys (this))))
  ((getFrameKey key)(let ((getter (get-slot-getter (this) key)))
                      (if (absent? getter)
                          (*:get slots key)
                          (getter (this) key))))
  ((setFrameKey key val)(let ((setter (get-slot-setter (this) key)))
                          (if (absent? setter)
                              (set! slots (*:plus slots key val))
                              (setter (this) key val))))
  ((deleteFrameKey key) (error "Cannot delete slots from a Player!")))

(define (make-player)
  (let* ((player (Player))
         (player-name (java.lang.System:getProperty "user.name"))
         (player-node (Node player-name))
         (player (Player)))
    (set-key! player name: player-name)
    (set-key! player node: player-node)
    player))

(define (init-player app)
  (let* ((root::Node (get-key app root-node:))
         (player (make-player))
         (player-node::Node (get-key player node:)))
    (set-key! app player: player)
    (*:setLocalTranslation player-node 0.0 8000.0 0.0)
    (let ((rotation (Quaternion))
          (pitch-axis (Vector3f 1 0 0)))
      ;; PI radians points us right at the center
      (*:fromAngleAxis rotation PI pitch-axis)
      (*:setLocalRotation player-node rotation)
      (*:attachChild root player-node))))

