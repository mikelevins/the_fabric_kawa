;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          particles.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       tools for building particle effects
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-plasma-generator make-plasma-generator-color make-any-plasma-generator)

(require "util-java.scm")
(require "assets-general.scm")
(require "view-colors.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias ColorRGBA com.jme3.math.ColorRGBA)
(define-private-alias PrMaterial com.jme3.material.Material)
(define-private-alias ParticleEmitter com.jme3.effect.ParticleEmitter)
(define-private-alias ParticleMesh com.jme3.effect.ParticleMesh)
(define-private-alias PrVector3f com.jme3.math.Vector3f)

;;; ---------------------------------------------------------------------
;;; particle-effect constructors
;;; ---------------------------------------------------------------------

(define (make-plasma-generator-color color::ColorRGBA)
  (let* ((found-r (@ 'getRed color))
         (found-g (@ 'getGreen color))
         (found-b (@ 'getBlue color))
         (faded-a 0.6))
    (ColorRGBA found-r found-g found-b faded-a)))

(define (make-plasma-generator color::ColorRGBA)
  (let* ((emitter (ParticleEmitter "Smoke" ParticleMesh:Type:Triangle 10))
         (mat (PrMaterial (get-asset-manager) "Common/MatDefs/Misc/Particle.j3md"))
         (end-color (fade color))
         (v (choose-any '(0.5 0.75 1 1.25 1.5))))
    (@ 'setTexture mat "Texture" (@ 'loadTexture (get-asset-manager) "Effects/Explosion/Debris.png"))
    (@ 'setMaterial emitter mat)
    (@ 'setImagesX emitter 3)
    (@ 'setImagesY emitter 3)
    (@ 'setEndColor emitter end-color)
    (@ 'setStartColor emitter color)
    (@ 'setInitialVelocity (@ 'getParticleInfluencer emitter)
                           (PrVector3f v v v))
    (@ 'setNumParticles emitter (choose-any '(6 12 18 24)))
    (@ 'setParticlesPerSec emitter (choose-any '(4 8 12)))
    (@ 'setSelectRandomImage emitter #t)
    (@ 'setRandomAngle emitter #t)
    (@ 'setStartSize emitter 4)
    (@ 'setEndSize emitter 0.1)
    (@ 'setGravity emitter 0 0 0)
    (@ 'setLowLife emitter 3)
    (@ 'setHighLife emitter 8)
    (@ 'setVelocityVariation (@ 'getParticleInfluencer emitter) 0.5)
    emitter))

(define (make-any-plasma-generator)
  (make-plasma-generator (any-lit-color)))
