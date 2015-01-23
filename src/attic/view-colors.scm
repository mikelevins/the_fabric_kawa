;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          colors.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       tools for coloring things
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export any-transparency any-lit-color any-glow brighten darken fade)

(require "util-java.scm")
(require "util-lists.scm")
(require "util-general.scm")

(import-as ColorRGBA com.jme3.math.ColorRGBA)

(define (any-transparency)
  (choose-any '( 0.4 0.5 0.6 0.7)))

(define (any-lit-color)
  (let ((transp (any-transparency))
        (red::float (choose-any '(0.25 0.5 0.75 1.0)))
        (green::float (choose-any '(0.25 0.5 0.75 1.0)))
        (blue::float (choose-any '(0.25 0.5 0.75 1.0))))
    (ColorRGBA red green blue transp)))

(define (brighten color::ColorRGBA)
  (let* ((found-r (*:getRed color))
         (bright-r (min 1.0 (* 3 found-r)))
         (found-g (*:getGreen color))
         (bright-g (min 1.0 (* 3 found-g)))
         (found-b (*:getBlue color))
         (bright-b (min 1.0 (* 3 found-b)))
         (bright-a 0.75))
    (ColorRGBA bright-r bright-g bright-b bright-a)))

(define (darken color::ColorRGBA)
  (let* ((found-r (*:getRed color))
         (dark-r (/ found-r 2.0))
         (found-g (*:getGreen color))
         (dark-g (/ found-g 2.0))
         (found-b (*:getBlue color))
         (dark-b (/ found-b 2.0))
         (dark-a 0.4))
    (ColorRGBA dark-r dark-g dark-b dark-a)))

(define (fade color::ColorRGBA)
  (let* ((found-r (*:getRed color))
         (found-g (*:getGreen color))
         (found-b (*:getBlue color))
         (faded-a 0.1))
    (ColorRGBA found-r found-g found-b faded-a)))

(define (any-glow color::ColorRGBA)
  (*:mult color
          (ColorRGBA (choose-any '(1.5 1.6 1.7 1.8 1.9 2.0 2.1 2.2 2.3 2.4 2.5
                                       2.6 2.7 2.8 2.9 3.0 3.1 3.2 3.4 3.5 3.6))
                     (choose-any '(1.5 1.6 1.7 1.8 1.9 2.0 2.1 2.2 2.3 2.4 2.5
                                       2.6 2.7 2.8 2.9 3.0 3.1 3.2 3.4 3.5 3.6))
                     (choose-any '(1.5 1.6 1.7 1.8 1.9 2.0 2.1 2.2 2.3 2.4 2.5
                                       2.6 2.7 2.8 2.9 3.0 3.1 3.2 3.4 3.5 3.6))
                     (choose-any '(0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6)))))


