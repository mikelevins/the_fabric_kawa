;;;; ***********************************************************************
;;;; Name:          setting-scene.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       set up the scene graph
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export init-scene)

(define-private-alias Mouse org.lwjgl.input.Mouse)

(define (init-scene app)
  (Mouse:setGrabbed #f)
  (set-key! app skybox: (make-client-sky))
  (set-key! app hub: (make-celestial-object (get-key app hub-name:))))
