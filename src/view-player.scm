;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          player.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       tools for building player characters
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-player-character make-armors player-namestring)

(require 'list-lib)
(require "util-java.scm")
(require "util-lists.scm")
(require "assets-general.scm")
(require "model-namegen.scm")
(require "view-name-cube.scm")
(require "model-entity.scm")
(require "model-frame.scm")
(require "view-shapes.scm")
(require "view-controls.scm")
(require "view-plasma.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Box com.jme3.scene.shape.Box)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Material com.jme3.material.Material)
(import-as RenderQueue com.jme3.renderer.queue.RenderQueue)
(import-as RenderState com.jme3.material.RenderState)
(import-as Sphere com.jme3.scene.shape.Sphere)

;;; ---------------------------------------------------------------------
;;; character armor
;;; ---------------------------------------------------------------------

;;; $available-armorers
;;; ---------------------------------------------------------------------
;;; armor constructors

(define $available-armorers
  (list make-enclosing-cube
        make-enclosing-wire-cube
        make-enclosing-pyramid
        make-enclosing-sphere
        make-enclosing-wire-sphere))

;;; (make-armors n)
;;; ---------------------------------------------------------------------
;;; make N randomly-chosen armors

(define (make-armors n)
  (let ((armorers (cons make-any-plasma-generator
                        (map (lambda (a)(choose-any $available-armorers))
                             (iota n)))))
    (map (lambda (make-armor)
           (let ((armor::Geometry (make-armor))
                 (rotator (any-rotator)))
             (*:addControl armor rotator)
             armor))
         armorers)))

;;; ---------------------------------------------------------------------
;;; construct a character
;;; ---------------------------------------------------------------------

(define (make-player-character lit-color)
  (let* ((name (gen-name))
         (name-bits (*:getData name))
         (cube (make-name-cube name-bits lit-color)))
    (entity name: name
            lit-color: lit-color
            name-cube: cube)))

;;; (make-player-character)

(define (player-namestring player)
  (let* ((name-strings (fabric-name-strings (get-key player name: "")))
         (out-string (call-with-output-string
                      (lambda (out)
                        (for-each (lambda (s)
                                    (format out "~a " s))
                                  name-strings)))))
    (*:toString out-string)))
