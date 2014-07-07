;;; ***********************************************************************
;;;; Name:          controls-client-camera.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       client camera control
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export init-camera)

(define-private-alias CameraControl com.jme3.scene.control.CameraControl)
(define-private-alias CameraNode com.jme3.scene.CameraNode)
(define-private-alias FlyByCamera com.jme3.input.FlyByCamera)
(define-private-alias Node com.jme3.scene.Node)
(define-private-alias Vector3f com.jme3.math.Vector3f)

(define (init-camera app player)
  (let* ((camera::com.jme3.renderer.Camera (get-key app camera:))
         (cam-node::CameraNode (CameraNode "camera" camera))
         (fly-cam::FlyByCamera (get-key app flyby-camera:))
         (player-node::Node (get-key player node:)))
    (*:setEnabled fly-cam #f)
    (*:setControlDir cam-node CameraControl:ControlDirection:SpatialToCamera)
    (*:setFrustumFar camera 30000)
    ;; position the camera behind and above the player and look at the player
    (*:setLocalTranslation cam-node (Vector3f 0 30 -30))
    (*:lookAt cam-node (*:getLocalTranslation player-node) Vector3f:UNIT_Y)
    ;; attach the camera to the player character
    (*:attachChild player-node cam-node)
    cam-node))
