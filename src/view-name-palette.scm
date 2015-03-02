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
 make-name-palette
 name-palette-get-name
 pick-a-random-name)

(require 'list-lib)
(require "util-lists.scm")
(require "util-random.scm")
(require "util-java.scm")
(require "appstate-character-creator.scm")
(require "data-names.scm")
(require "view-name-menu.scm")
(require "view-random-name-button.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as MenuItem tonegod.gui.controls.menuing.MenuItem)
(import-as Screen tonegod.gui.core.Screen)
(import-as SelectBox tonegod.gui.controls.lists.SelectBox)
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
         (adjusted-segment-width (- segment-width 24))
         (lefts (map (lambda (i)
                       (+ 28
                          (* i 24)
                          (* i adjusted-segment-width)))
                     (iota 8)))
         (tops (map (lambda (i) 40)
                    (iota 8)))
         (widths (map (lambda (i) adjusted-segment-width)
                      (iota 8)))
         (heights (map (lambda (i) 40)
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
             (random-name-button (make-random-name-button palette screen))
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
        (*:addChild palette random-name-button)
        palette))))


(define (pick-a-random-name palette::Window)
  (let* ((dom0-menu::NameMenu (*:getChildElementById palette "Domain0Menu"))
         (dom1-menu::NameMenu (*:getChildElementById palette "Domain1Menu"))
         (dom2-menu::NameMenu (*:getChildElementById palette "Domain2Menu"))
         (dom3-menu::NameMenu (*:getChildElementById palette "Domain3Menu"))
         (dom4-menu::NameMenu (*:getChildElementById palette "Domain4Menu"))
         (dom5-menu::NameMenu (*:getChildElementById palette "Domain5Menu"))
         (dom6-menu::NameMenu (*:getChildElementById palette "Domain6Menu"))
         (dom7-menu::NameMenu (*:getChildElementById palette "Domain7Menu"))
         (all-menus (list dom0-menu dom1-menu dom2-menu dom3-menu
                          dom4-menu dom5-menu dom6-menu dom7-menu))
         (chosen-menus (any-n (+ 1 (random-integer (length all-menus))) all-menus)))
    (for-each (lambda (menu::SelectBox)
                (*:setSelectedIndex menu 0)
                (let ((value (*:getSelectedListItem menu)))
                  (*:onChange menu 0 value)))
              all-menus)
    (for-each (lambda (menu::SelectBox)
                (let ((index (+ 1 (random-integer 63))))
                  (*:setSelectedIndex menu index)
                  (let ((value (*:getSelectedListItem menu)))
                    (*:onChange menu index value))))
              chosen-menus)))

;;; (name-palette-get-name palette::Window)
;;; ---------------------------------------------------------------------
;;; returns the FabricName currently displayed in the name palette

(define (name-palette-get-name palette::Window)
  (let* ((dom0-menu::NameMenu (*:getChildElementById palette "Domain0Menu"))
         (dom0-item::MenuItem (*:getSelectedListItem dom0-menu))
         (dom0-value (*:getValue dom0-item))
         (dom1-menu::NameMenu (*:getChildElementById palette "Domain1Menu"))
         (dom1-item::MenuItem (*:getSelectedListItem dom1-menu))
         (dom1-value (*:getValue dom1-item))
         (dom2-menu::NameMenu (*:getChildElementById palette "Domain2Menu"))
         (dom2-item::MenuItem (*:getSelectedListItem dom2-menu))
         (dom2-value (*:getValue dom2-item))
         (dom3-menu::NameMenu (*:getChildElementById palette "Domain3Menu"))
         (dom3-item::MenuItem (*:getSelectedListItem dom3-menu))
         (dom3-value (*:getValue dom3-item))
         (dom4-menu::NameMenu (*:getChildElementById palette "Domain4Menu"))
         (dom4-item::MenuItem (*:getSelectedListItem dom4-menu))
         (dom4-value (*:getValue dom4-item))
         (dom5-menu::NameMenu (*:getChildElementById palette "Domain5Menu"))
         (dom5-item::MenuItem (*:getSelectedListItem dom5-menu))
         (dom5-value (*:getValue dom5-item))
         (dom6-menu::NameMenu (*:getChildElementById palette "Domain6Menu"))
         (dom6-item::MenuItem (*:getSelectedListItem dom6-menu))
         (dom6-value (*:getValue dom6-item))
         (dom7-menu::NameMenu (*:getChildElementById palette "Domain7Menu"))
         (dom7-item::MenuItem (*:getSelectedListItem dom7-menu))
         (dom7-value (*:getValue dom7-item)))
    (list dom0-value dom1-value dom2-value dom3-value
          dom4-value dom5-value dom6-value dom7-value)))
