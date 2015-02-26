;;;; ***********************************************************************
;;;;
;;;; Name:          view-name-palette.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the name palette for the character creator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-name-palette-origin
 compute-name-palette-size
 make-name-palette)

(require 'list-lib)
(require "appstate-character-creator.scm")
(require "data-names.scm")
(require "view-name-menu.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)

;;; =====================================================================
;;; the name palette
;;; =====================================================================


;;; (compute-name-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the name palette

(define (compute-name-palette-origin screen::Screen)
  (let ((height (*:getHeight screen)))
    (Vector2f 10 (- height 180))))


;;; (compute-name-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the name palette

(define (compute-name-palette-size screen::Screen)
  (let ((width (*:getWidth screen)))
    (Vector2f (- width 20) 160)))


;;; (compute-name-menu-bounds size)
;;; ---------------------------------------------------------------------
;;; computes and returns lists of coordinates to be used as the bounds
;;; of name menus in the name palette

(define (compute-name-menu-bounds size::Vector2f)
  (let* ((width (*:getX size))
         (adjusted-width (- width 16))
         (segment-width (/ adjusted-width 8))
         (adjusted-segment-width (- segment-width 32))
         (lefts (map (lambda (i)
                       (+ 48
                          (* i 24)
                          (* i adjusted-segment-width)))
                     (iota 8)))
         (tops (map (lambda (i) 40)
                    (iota 8)))
         (widths (map (lambda (i) adjusted-segment-width)
                      (iota 8)))
         (heights (map (lambda (i) 24)
                       (iota 8))))
    (values lefts tops widths heights)))


;;; (make-name-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed and -populated name palette

(define (make-name-palette app-state::CharacterCreatorAppState screen::Screen)
  (let ((size::Vector2f (compute-name-palette-size screen)))
    (receive (lefts tops widths heights) (compute-name-menu-bounds size)
      (let* ((origin (compute-name-palette-origin screen))
             (palette (Window screen "NamePalette" origin size))
             (indexes (iota (length lefts)))
             (%make-menu (lambda (i)
                           (let* ((dom (list-ref (name-domains) i))
                                  (dom-menu::NameMenu
                                   (NameMenu app-state
                                             screen (format #f "Domain~dMenu" i)
                                             (Vector2f (list-ref lefts i)(list-ref tops i))
                                             (Vector2f (list-ref widths i)(list-ref heights i))))
                                  (%add-item (lambda (nm)(*:addListItem dom-menu nm nm))))
                             (for-each %add-item dom)
                             (*:addChild palette dom-menu)))))
        (for-each %make-menu indexes)
        palette))))

