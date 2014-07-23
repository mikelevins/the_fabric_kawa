;;;; ***********************************************************************
;;;; Name:          ui-workshop-palette.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the palette window
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-palette)

(require 'list-lib)
(require "interface-frame.scm")
(require "application-common.scm")

(define-private-alias AppSettings com.jme3.system.AppSettings)
(define-private-alias ButtonAdapter tonegod.gui.controls.buttons.ButtonAdapter)
(define-private-alias Label tonegod.gui.controls.text.Label)
(define-private-alias Procedure gnu.mapping.Procedure)
(define-private-alias Screen tonegod.gui.core.Screen)
(define-private-alias String java.lang.String)
(define-private-alias Vector2f com.jme3.math.Vector2f)
(define-private-alias Window tonegod.gui.controls.windows.Window)

;;; palette button

(define-simple-class PaletteButton (ButtonAdapter)
  (application init-form: #!null)
  (name init-form: #!null)
  ;; (lambda (button app event)...)
  (click-handler init-form: #!null)

  ((*init* app::FabricApp screen::Screen button-name::String position::Vector2f handler::Procedure)
   (invoke-special ButtonAdapter (this) '*init* screen button-name position)
   (set! application app)
   (set! name button-name)
   (set! click-handler handler))
  ((getName) name)
  ((onMouseLeftReleased event)(unless (absent? click-handler)
                                (click-handler (this) application event))))

;;; make the palette window

(define (make-palette app screen)
  (let* ((screen (get-key app gui-screen:))
         (settings::AppSettings (get-key app settings:))
         (screen-margin 8)
         (palette-width 144)
         (palette-height (- (*:getHeight settings)
                            176
                            (* 2 screen-margin)))
         (palette-left screen-margin)
         (palette-top screen-margin)
         (win (Window screen "Palette"
                      (Vector2f palette-left palette-top)
                      (Vector2f palette-width palette-height)))
         (hubs-label (Label screen "Hubs" (Vector2f 16 24)(Vector2f 100 32)))
         (hnames (hub-names))
         (hoffsets (map (lambda (n)(+ 60 (* n 40)))
                        (iota (length hnames))))
         (hub-buttons (map (lambda (nm::java.lang.String i)
                             (let* ((handler (lambda (button::PaletteButton app event)
                                               (set-key! app focus-object: (*:getName button))))
                                    (btn (PaletteButton app screen nm (Vector2f 32 i) handler)))
                               (*:setText btn nm)
                               btn))
                           hnames hoffsets)))
    (*:setText hubs-label "Hubs")
    (*:addChild win hubs-label)
    (for-each (lambda (btn)(*:addChild win btn))
              hub-buttons)
    win))
