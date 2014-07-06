;;; ***********************************************************************
;;;; Name:          controls-client-inputs.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       client input-handling
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export init-inputs handle-client-analog-event
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

;;; (handle-client-analog-event app name value tpf)
;;; ---------------------------------------------------------------------
;;; handle mouse movements and other continuous events

(define (handle-client-analog-event app name value tpf)
  (let* ((player (get-key app player:))
         (player-node (get-key player node:))
         (right-button? (get-key app right-button:)))
    (on-analog (name)
               ("moveForward" -> (player-move! app (* 600 tpf)))
               ("maybeMoveForward" -> (when right-button?
                                        (player-move! app (* 600 tpf))))
               ("moveBackward" -> (player-move! app (* -400 tpf)))
               ("moveRight" -> (player-strafe! app (* -300 tpf)))
               ("moveLeft" -> (player-strafe! app (* 300 tpf)))
               ("rotateRight" -> (*:rotate player-node 0 0 (* 0.5 tpf)))
               ("mouseRotateRight" -> (when right-button?
                                        (*:rotate player-node 0 (* 0.5 tpf) 0)))
               ("rotateLeft" -> (*:rotate player-node 0 0 (* -0.5 tpf)))
               ("mouseRotateLeft" -> (when right-button?
                                       (*:rotate player-node 0 (* -0.5 tpf) 0)))
               ("rotateUp" -> (*:rotate player-node (* -0.5 tpf) 0 0))
               ("mouseRotateUp" -> (when right-button?
                                     (*:rotate player-node (* -1 value) 0 0)))
               ("rotateDown" -> (*:rotate player-node (* 0.5 tpf) 0 0))
               ("mouseRotateDown" -> (when right-button?
                                       (*:rotate player-node (* 1 value) 0 0))))))

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
              (list (input-spec "rotateUp" (KeyTrigger key-input:KEY_UP))
                    (input-spec "moveForward" (KeyTrigger key-input:KEY_W))
                    (input-spec "maybeMoveForward" (MouseButtonTrigger MouseInput:BUTTON_LEFT))
                    (input-spec "leftButton" (MouseButtonTrigger MouseInput:BUTTON_LEFT))
                    (input-spec "rightButton" (MouseButtonTrigger MouseInput:BUTTON_RIGHT))
                    (input-spec "rotateRight" (KeyTrigger key-input:KEY_RIGHT))
                    (input-spec "moveRight" (KeyTrigger key-input:KEY_D))
                    (input-spec "mouseRotateRight" (MouseAxisTrigger 0 #f))
                    (input-spec "rotateLeft" (KeyTrigger key-input:KEY_LEFT))
                    (input-spec "moveLeft" (KeyTrigger key-input:KEY_A))
                    (input-spec "mouseRotateLeft" (MouseAxisTrigger 0 #t))
                    (input-spec "mouseRotateUp" (MouseAxisTrigger 1 #f))
                    (input-spec "rotateDown" (KeyTrigger key-input:KEY_DOWN))
                    (input-spec "moveBackward" (KeyTrigger key-input:KEY_S))
                    (input-spec "mouseRotateDown" (MouseAxisTrigger 1 #t))))))




