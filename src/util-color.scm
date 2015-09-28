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
 bright-abjurers-color
 bright-caretakers-color
 bright-rogues-color
 brighten-color
 darken-color
 default-character-color
 default-glow-color
 dim-abjurers-color
 dim-caretakers-color
 dim-rogues-color
 faction-dim-color
 faction-lit-color)

(import (class com.jme3.math ColorRGBA))

(define default-character-color
  (make-parameter (ColorRGBA 0.25 0.25 0.25 0.25)))

(define default-glow-color
  (make-parameter (ColorRGBA 0.75 0.75 0.75 0.5)))


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

(define bright-caretakers-color (make-parameter (ColorRGBA 0.6 1.0 0.6 0.6)))
(define dim-caretakers-color (make-parameter (ColorRGBA 0.1 0.4 0.1 0.3)))
(define bright-rogues-color (make-parameter (ColorRGBA 0.4 0.8 1.0 0.6)))
(define dim-rogues-color (make-parameter (ColorRGBA 0.0 0.3 0.6 0.3)))
(define bright-abjurers-color (make-parameter (ColorRGBA 1.0 0.0 0.0 0.6)))
(define dim-abjurers-color (make-parameter (ColorRGBA 0.4 0.0 0.0 0.2)))

(define (faction-lit-color faction-name)
  (if (eqv? #!null faction-name)
      (default-glow-color)
      (case faction-name
        ((caretakers)(bright-caretakers-color))
        ((rogues)(bright-rogues-color))
        ((abjurers)(bright-abjurers-color))
        (else (default-glow-color)))))

(define (faction-dim-color faction-name)
  (if (eqv? #!null faction-name)
      (default-character-color)
      (case faction-name
        ((caretakers)(dim-caretakers-color))
        ((rogues)(dim-rogues-color))
        ((abjurers)(dim-abjurers-color))
        (else (default-character-color)))))
