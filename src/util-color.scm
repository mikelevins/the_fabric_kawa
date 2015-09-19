;;;; ***********************************************************************
;;;;
;;;; Name:          colors.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       tools for coloring things
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 brighten-color
 darken-color)

(require util-java)

(import-as ColorRGBA com.jme3.math.ColorRGBA)


;;; (brighten color::ColorRGBA)
;;; ---------------------------------------------------------------------
;;; returns a brighter version of color

(define (brighten-color color::ColorRGBA)
  (let* ((found-r (*:getRed color))
         (bright-r (min 1.0 (* 3 found-r)))
         (found-g (*:getGreen color))
         (bright-g (min 1.0 (* 3 found-g)))
         (found-b (*:getBlue color))
         (bright-b (min 1.0 (* 3 found-b)))
         (bright-a 0.75))
    (ColorRGBA bright-r bright-g bright-b bright-a)))

;;; (darken color::ColorRGBA)
;;; ---------------------------------------------------------------------
;;; returns a darker version of color

(define (darken-color color::ColorRGBA)
  (let* ((found-r (*:getRed color))
         (dark-r (/ found-r 2.0))
         (found-g (*:getGreen color))
         (dark-g (/ found-g 2.0))
         (found-b (*:getBlue color))
         (dark-b (/ found-b 2.0))
         (dark-a 0.4))
    (ColorRGBA dark-r dark-g dark-b dark-a)))
