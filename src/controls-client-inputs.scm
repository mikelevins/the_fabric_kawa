;;; ***********************************************************************
;;;; Name:          controls-client-inputs.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       client input-handling
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export init-inputs handle-client-analog-event handle-action-event
               input-spec-name input-spec-signal)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "syntax-events.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias Camera com.jme3.renderer.Camera)
(define-private-alias KeyInput com.jme3.input.KeyInput)
(define-private-alias KeyTrigger com.jme3.input.controls.KeyTrigger)
(define-private-alias MouseAxisTrigger com.jme3.input.controls.MouseAxisTrigger)
(define-private-alias MouseButtonTrigger com.jme3.input.controls.MouseButtonTrigger)
(define-private-alias MouseInput com.jme3.input.MouseInput)

;;; ---------------------------------------------------------------------
;;; event-handling
;;; ---------------------------------------------------------------------

(define (player-move! app distance)
  (let* ((camera::Camera (get-key app camera:))
         (player (get-key app player:))
         (player-node (get-key player node:)))
    (*:normalizeLocal (*:getDirection camera))
    (set-key! app direction: (*:getDirection camera))
    (*:multLocal (get-key app direction:) distance)
    (*:move (get-key player node:) (get-key app direction:))))

(define (player-strafe! app distance)
  (let* ((camera::Camera (get-key app camera:))
         (player (get-key app player:))
         (player-node (get-key player node:)))
    (set-key! app direction: (*:normalizeLocal (*:getLeft camera)))
    (*:multLocal (get-key app direction:) distance)
    (*:move player-node (get-key app direction:))))

(define (player-rise! app distance)
  (let* ((camera::Camera (get-key app camera:))
         (player (get-key app player:))
         (player-node (get-key player node:)))
    (set-key! app direction: (*:normalizeLocal (*:getUp camera)))
    (*:multLocal (get-key app direction:) distance)
    (*:move player-node (get-key app direction:))))

;;; (handle-client-analog-event app name value tpf)
;;; ---------------------------------------------------------------------
;;; handle mouse movements and other continuous events

(define (handle-client-analog-event app name value tpf)
  (let* ((player (get-key app player:))
         (player-node (get-key player node:))
         (right-button? (get-key app right-button:)))
    (on-analog (name)
               ("rotateUp" -> (*:rotate player-node (* -0.5 tpf) 0 0))
               ("rotateDown" -> (*:rotate player-node (* 0.5 tpf) 0 0))
               ("rotateRight" -> (*:rotate player-node 0 0 (* 0.5 tpf)))
               ("rotateLeft" -> (*:rotate player-node 0 0 (* -0.5 tpf)))
               ("mouseRotateUp" -> (when right-button? (*:rotate player-node (* -1 value) 0 0)))
               ("mouseRotateDown" -> (when right-button? (*:rotate player-node (* 1 value) 0 0)))
               ("mouseRotateRight" -> (when right-button? (*:rotate player-node 0 0 (* 2.5 tpf))))
               ("mouseRotateLeft" -> (when right-button? (*:rotate player-node 0 0 (* -2.5 tpf))))
               ("maybeMoveForward" -> (when right-button? (player-move! app (* 800 tpf))))
               ("moveUp" -> (player-rise! app (* 600 tpf)))
               ("moveDown" -> (player-rise! app (* -600 tpf)))
               ("moveForward" -> (player-move! app (* 800 tpf)))
               ("moveBackward" -> (player-move! app (* -600 tpf)))
               ("moveRight" -> (player-strafe! app (* -600 tpf)))
               ("moveLeft" -> (player-strafe! app (* 600 tpf)))
               )))

;;; (handle-action-event app name key-pressed? tpf)
;;; ---------------------------------------------------------------------
;;; handle keypresses, mouse clicks, and other discrete events

(define (handle-action-event app name key-pressed? tpf)
  (on-action (name)
             ("leftButton" -> (*:setLeftButton app key-pressed?))
             ("rightButton" -> (*:setRightButton app key-pressed?))))

(define (input-spec name signal)
  (list name signal))

(define (input-spec-name spec)
  (car spec))

(define (input-spec-signal spec)
  (cadr spec))

(define (init-inputs app)
  ;; set up the player's controls
  (let ((key-input ::KeyInput (get-key app key-input:)))
    (set-key! app inputs:
              (list (input-spec "leftButton" (MouseButtonTrigger MouseInput:BUTTON_LEFT))
                    (input-spec "rightButton" (MouseButtonTrigger MouseInput:BUTTON_RIGHT))
                    (input-spec "rotateUp" (KeyTrigger key-input:KEY_UP))
                    (input-spec "rotateDown" (KeyTrigger key-input:KEY_DOWN))
                    (input-spec "rotateRight" (KeyTrigger key-input:KEY_RIGHT))
                    (input-spec "rotateLeft" (KeyTrigger key-input:KEY_LEFT))
                    (input-spec "mouseRotateUp" (MouseAxisTrigger 1 #f))
                    (input-spec "mouseRotateDown" (MouseAxisTrigger 1 #t))
                    (input-spec "mouseRotateRight" (MouseAxisTrigger 0 #f))
                    (input-spec "mouseRotateLeft" (MouseAxisTrigger 0 #t))
                    (input-spec "maybeMoveForward" (MouseButtonTrigger MouseInput:BUTTON_LEFT))
                    (input-spec "moveUp" (KeyTrigger key-input:KEY_Q))
                    (input-spec "moveDown" (KeyTrigger key-input:KEY_Z))
                    (input-spec "moveForward" (KeyTrigger key-input:KEY_W))
                    (input-spec "moveBackward" (KeyTrigger key-input:KEY_S))
                    (input-spec "moveRight" (KeyTrigger key-input:KEY_D))
                    (input-spec "moveLeft" (KeyTrigger key-input:KEY_A))))))




