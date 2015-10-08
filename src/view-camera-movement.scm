;;;; ***********************************************************************
;;;;
;;;; Name:          view-camera-movement.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       an API for controlling camera movement
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 move-node!
 move-node-backward!
 move-node-down!
 move-node-forward!
 move-node-left!
 move-node-right!
 move-node-up!
 normalize-camera!
 rotate-node-down!
 rotate-node-left!
 rotate-node-right!
 rotate-node-up!)

(require client)

(import (class com.jme3.math Vector3f))
(import (class com.jme3.scene Node))

;;; (normalize-camera! app :: FabricClient)
;;; ---------------------------------------------------------------------
;;; orients the camera to where the player's node is facing

(define (normalize-camera! app :: FabricClient)
  (let ((dir :: Vector3f (*:getCameraDirection app)))
    (*:normalizeLocal dir)))

;;; (move-node!  app :: FabricClient node :: Node amount :: float invert?)
;;; ---------------------------------------------------------------------
;;; moves  _node_  a distance along an arbitrary vector.
;;; used by more specific move- functions like move-node-forward!

(define (move-node!  app :: FabricClient node :: Node amount :: float invert?)
  (let ((dir :: Vector3f app:direction)
        (sign (if invert? -1 1)))
    (*:multLocal dir (* sign amount))
    (*:move node dir)))


;;; (move-node-forward! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ forward a distance of _amount_

(define (move-node-forward! app :: FabricClient node :: Node amount :: float)
  (normalize-camera! app)
  (set! app:direction (*:getCameraDirection app))
  (move-node! app node amount #f))


;;; (move-node-backward! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ backward a distance of _amount_

(define (move-node-backward! app :: FabricClient node :: Node amount :: float)
  (normalize-camera! app)
  (set! app:direction (*:getCameraDirection app))
  (move-node! app node amount #t))


;;; (move-node-left! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ to the left a distance of _amount_

(define (move-node-left! app :: FabricClient node :: Node amount :: float)
  (set! app:direction (*:normalizeLocal (*:getLeft (*:getCamera app))))
  (move-node! app node amount #f))


;;; (move-node-right! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ to the right a distance of _amount_

(define (move-node-right! app :: FabricClient node :: Node amount :: float)
  (set! app:direction (*:normalizeLocal (*:getLeft (*:getCamera app))))
  (move-node! app node amount #t))

;;; (move-node-up! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ upward a distance of _amount_

(define (move-node-up! app :: FabricClient node :: Node amount :: float)
  (set! app:direction (*:normalizeLocal (*:getUp (*:getCamera app))))
  (move-node! app node (* -1 amount) #t))

;;; (move-node-down! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ downward a distance of _amount_

(define (move-node-down! app :: FabricClient node :: Node amount :: float)
  (set! app:direction (*:normalizeLocal (*:getUp (*:getCamera app))))
  (move-node! app node amount #t))


;;; (rotate-node-right! node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; rotates _node_ to the right an angle of _amount_ 

(define (rotate-node-right! node :: Node amount :: float)
  (*:rotate node 0 (* -1 amount) 0))


;;; (rotate-node-left! node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; rotates _node_ to the left an angle of _amount_ 

(define (rotate-node-left! node :: Node amount :: float)
  (*:rotate node 0 (* 1 amount) 0))


;;; (rotate-node-up! node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; rotates _node_ upward an angle of _amount_ 

(define (rotate-node-up! node :: Node amount :: float)
  (*:rotate node (* -1 amount) 0 0))


;;; (rotate-node-down! node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; rotates _node_ downward an angle of _amount_ 

(define (rotate-node-down! node :: Node amount :: float)
  (*:rotate node amount 0 0))
