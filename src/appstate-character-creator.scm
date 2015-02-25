;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-character-creator.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       create a new character
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 CharacterCreatorAppState)

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; the character creator provides players the ability to create and
;;; customize new playable characters

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "util-java.scm")
(require "syntax-classes.scm")
(require "data-names.scm")
(require "view-player-character.scm")
(require "client-main.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as AbstractControl com.jme3.scene.control.AbstractControl)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as AssetManager com.jme3.asset.AssetManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as Button tonegod.gui.controls.buttons.Button)
(import-as ButtonAdapter tonegod.gui.controls.buttons.ButtonAdapter)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as ComboBox tonegod.gui.controls.lists.ComboBox)
(import-as Integer java.lang.Integer)
(import-as Label tonegod.gui.controls.text.Label)
(import-as MouseButtonEvent com.jme3.input.event.MouseButtonEvent)
(import-as Node com.jme3.scene.Node)
(import-as Panel tonegod.gui.controls.windows.Panel)
(import-as RadioButton tonegod.gui.controls.buttons.RadioButton)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)
(import-as SelectBox tonegod.gui.controls.lists.SelectBox)
(import-as Serializer com.jme3.network.serializing.Serializer)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as SkyFactory com.jme3.util.SkyFactory)
(import-as TLabel tonegod.gui.controls.text.Label)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector4f com.jme3.math.Vector4f)
(import-as Window tonegod.gui.controls.windows.Window)



;;; =====================================================================
;;; UI elements
;;; =====================================================================

;;; CLASS NameMenu
;;; ---------------------------------------------------------------------
;;; a SelectBox subclass used to present name options for player
;;; characters

(defclass NameMenu (SelectBox)
  (slots:
   (app-state::CharacterCreatorAppState init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* state::CharacterCreatorAppState screen::Screen uid::String position::Vector2f size::Vector2f)
    (invoke-special SelectBox (this) '*init* screen uid position size)
    (set! app-state state))
   ((onChange index::int value::Object)
    (notify-name-selection-changed app-state index value))))

;;; CLASS FactionButtonGroup
;;; ---------------------------------------------------------------------
;;; a RadioButtonGroup subclass used to present faction options to
;;; players

(defclass FactionButtonGroup (RadioButtonGroup)
  (slots:
   (app-state init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (let ((button-id (*:getUID value))
          (state::CharacterCreatorAppState (get-app-state (this))))
      (cond
       ((equal? "CaretakerButton" button-id)(set-current-faction state 'caretakers))
       ((equal? "RogueButton" button-id)(set-current-faction state 'rogues))
       ((equal? "AbjurerButton" button-id)(set-current-faction state 'abjurers))
       (else (format #t "~%Unknown faction selected")))))))

;;; (get-app-state group::FactionButtonGroup)
;;; ---------------------------------------------------------------------
;;; returns _group_'s AppState object 

(define (get-app-state group::FactionButtonGroup)
  (*:getAppState group))

;;; (set-app-state! group::FactionButtonGroup state::CharacterCreatorAppState)
;;; ---------------------------------------------------------------------
;;; assigns _state_ to _group_'s app-state slot

(define (set-app-state! group::FactionButtonGroup state::CharacterCreatorAppState)
  (*:setAppState group state))



;;; =====================================================================
;;; the skybox
;;; =====================================================================

;;; (make-sky app::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed Fabric skybox

(define (make-sky app::SimpleApplication)
  (let ((asset-manager::AssetManager (get-asset-manager)))
    (SkyFactory:createSky asset-manager 
                          (*:loadTexture asset-manager "Textures/tycholeft.png")
                          (*:loadTexture asset-manager "Textures/tychoright.png")
                          (*:loadTexture asset-manager "Textures/tychofront.png")
                          (*:loadTexture asset-manager "Textures/tychoback.png")
                          (*:loadTexture asset-manager "Textures/tychotop.png")
                          (*:loadTexture asset-manager "Textures/tychobottom.png"))))




;;; =====================================================================
;;; the character nameplate
;;; =====================================================================

;;; (compute-character-nameplate-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the character nameplate,
;;; taking into accoun the dimensions of the screen

(define (compute-character-nameplate-origin screen::Screen)
  (let ((weapons-palette-size (compute-weapons-palette-size screen))
        (weapons-palette-origin (compute-weapons-palette-origin screen))
        (nameplate-size (compute-character-nameplate-size screen)))
    (Vector2f (+ 32 (*:getX weapons-palette-size))
              (- (+ (*:getY weapons-palette-origin)
                    (*:getY weapons-palette-size))
                 (*:getY nameplate-size)
                 8))))


;;; (compute-character-nameplate-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the character nameplate,
;;; taking into accoun the dimensions of the screen

(define (compute-character-nameplate-size screen::Screen)
  (let ((width (*:getWidth screen)))
    (Vector2f 280 40)))


;;; (make-character-nameplate screen::Screen)
;;; ---------------------------------------------------------------------
;;; constructs and returns a new TLabel object for use as the character
;;; nameplate

(define (make-character-nameplate screen::Screen)
  (let ((label (TLabel screen "CharacterNameplate"
                       (compute-character-nameplate-origin screen)
                       (compute-character-nameplate-size screen)))
        (Align BitmapFont:Align))
    (*:setTextAlign label Align:Center)
    label))



;;; =====================================================================
;;; the faction nameplate
;;; =====================================================================

;;; (compute-faction-nameplate-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the faction nameplate,
;;; taking into accoun the dimensions of the screen

(define (compute-faction-nameplate-origin screen::Screen)
  (let ((faction-palette-size (compute-faction-palette-size screen))
        (height (*:getHeight screen)))
    (Vector2f (+ 32 (*:getX faction-palette-size)) 8)))


;;; (compute-faction-nameplate-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the faction nameplate,
;;; taking into accoun the dimensions of the screen

(define (compute-faction-nameplate-size screen::Screen)
  (let ((width (*:getWidth screen)))
    (Vector2f 280 40)))


;;; (make-faction-nameplate screen::Screen)
;;; ---------------------------------------------------------------------
;;; constructs and returns a new TLabel object for use as the faction
;;; nameplate

(define (make-faction-nameplate screen::Screen)
  (let ((label (TLabel screen "FactionNameplate"
                       (compute-faction-nameplate-origin screen)
                       (compute-faction-nameplate-size screen)))
        (Align BitmapFont:Align))
    (*:setTextAlign label Align:Center)
    label))



;;; =====================================================================
;;; the faction palette
;;; =====================================================================

;;; (compute-faction-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the faction palette,
;;; taking into accoun the dimensions of the screen

(define (compute-faction-palette-origin screen::Screen)
  (Vector2f 10 10))


;;; (compute-faction-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the faction palette,
;;; taking into accoun the dimensions of the screen


(define (compute-faction-palette-size screen::Screen)
  (Vector2f 616 200))


;;; (make-faction-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed and -populated faction-palette window

(define (make-faction-palette screen::Screen)
  (Window screen "FactionPalette"
          (compute-faction-palette-origin screen)
          (compute-faction-palette-size screen)))


;;; (compute-caretaker-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the caretakers faction
;;; button


(define (compute-caretaker-button-origin screen::Screen)
  (let* ((button-width 128)
         (button-height 96)
         (palette-size (compute-faction-palette-size screen))
         (palette-width (*:getX palette-size))
         (palette-height (*:getY palette-size))
         (x (- (/ palette-width 6.0)
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))


;;; (compute-caretaker-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the caretakers faction
;;; button


(define (compute-caretaker-button-size screen::Screen)
  (Vector2f 128 96))


;;; (make-caretaker-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed caretakers faction button

(define (make-caretaker-button screen::Screen)
  (RadioButton screen "CaretakerButton"
               (compute-caretaker-button-origin screen)
               (compute-caretaker-button-size screen)))


;;; (compute-abjurer-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the abjurers faction
;;; button

(define (compute-abjurer-button-origin screen::Screen)
  (let* ((button-width 128)
         (button-height 96)
         (palette-size (compute-faction-palette-size screen))
         (palette-width (*:getX palette-size))
         (palette-height (*:getY palette-size))
         (x (- (* 3 (/ palette-width 6.0))
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))


;;; (compute-abjurer-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the abjurers faction
;;; button

(define (compute-abjurer-button-size screen::Screen)
  (Vector2f 128 96))


;;; (make-abjurer-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed abjurers faction button

(define (make-abjurer-button screen::Screen)
  (RadioButton screen "AbjurerButton"
               (compute-abjurer-button-origin screen)
               (compute-abjurer-button-size screen)))


;;; (compute-rogue-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the rogues faction
;;; button

(define (compute-rogue-button-origin screen::Screen)
  (let* ((button-width 128)
         (button-height 96)
         (palette-size (compute-faction-palette-size screen))
         (palette-width (*:getX palette-size))
         (palette-height (*:getY palette-size))
         (x (- (* 5 (/ palette-width 6.0))
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))


;;; (compute-rogue-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the rogues faction
;;; button

(define (compute-rogue-button-size screen::Screen)
  (Vector2f 128 96))


;;; (make-rogue-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed rogues faction button

(define (make-rogue-button screen::Screen)
  (RadioButton screen "RogueButton"
               (compute-rogue-button-origin screen)
               (compute-rogue-button-size screen)))



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
  (let ((size (compute-name-palette-size screen)))
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


;;; =====================================================================
;;; the weapons palette
;;; =====================================================================

;;; CLASS WeaponsButtonGroup
;;; ---------------------------------------------------------------------
;;; a RadioButtonGroup subclass used to present weapons options to
;;; players

(defclass WeaponsButtonGroup (RadioButtonGroup)
  (slots:
   (app-state init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (let ((button-id (*:getUID value))
          (state::CharacterCreatorAppState app-state))
      (cond
       ((equal? "CannonWeaponsButton" button-id)(set-current-weapon state 'cannon-weapons))
       (else (format #t "~%Unknown weapon selected")))))))

;;; (compute-weapons-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the weapons palette, taking
;;; into account the dimensions of the screen

(define (compute-weapons-palette-origin screen::Screen)
  (let ((faction-palette-origin (compute-faction-palette-origin screen))
        (faction-palette-size (compute-faction-palette-size screen)))
    (Vector2f 10
              (+ (*:getY faction-palette-origin)
                 (*:getY faction-palette-size)
                 8))))


;;; (compute-weapons-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the weapons palette, taking
;;; into account the dimensions of the screen

(define (compute-weapons-palette-size screen::Screen)
  (let* ((weapons-palette-origin (compute-weapons-palette-origin screen))
         (name-palette-origin (compute-name-palette-origin screen))
         (height (- (*:getY name-palette-origin)
                    (*:getY weapons-palette-origin)
                    8))
         (screen-width (*:getWidth screen))
         (width (- (/ screen-width 8.0) 10)))
    (Vector2f width height)))


;;; (make-weapons-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-contructed weapons palette

(define (make-weapons-palette screen::Screen)
  (Window screen "WeaponsPalette"
          (compute-weapons-palette-origin screen)
          (compute-weapons-palette-size screen)))


;;; (compute-cannon-weapon-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the cannon weapon
;;; button


(define (compute-cannon-weapon-button-origin screen::Screen)
  (let ((weapons-palette-size (compute-weapons-palette-size screen))
        (weapon-button-size (compute-cannon-weapon-button-size screen)))
    (Vector2f (- (/ (*:getX weapons-palette-size) 2.0)
                 (/ (*:getX weapon-button-size) 2.0))
              (- (/ (*:getY weapons-palette-size) 8.0)
                 (/ (*:getY weapon-button-size) 2.0)))))


;;; (compute-cannon-weapon-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the cannon weapon
;;; button


(define (compute-cannon-weapon-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-cannon-weapon-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed cannon weapon button

(define (make-cannon-weapon-button screen::Screen)
  (RadioButton screen "CannonWeaponButton"
               (compute-cannon-weapon-button-origin screen)
               (compute-cannon-weapon-button-size screen)))


;;; (compute-impulse-weapon-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the impulse weapon
;;; button


(define (compute-impulse-weapon-button-origin screen::Screen)
  (let ((weapons-palette-size (compute-weapons-palette-size screen))
        (weapon-button-size (compute-cannon-weapon-button-size screen)))
    (Vector2f (- (/ (*:getX weapons-palette-size) 2.0)
                 (/ (*:getX weapon-button-size) 2.0))
              (- (/ (* 3 (*:getY weapons-palette-size)) 8.0)
                 (/ (*:getY weapon-button-size) 2.0)))))


;;; (compute-impulse-weapon-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the impulse weapon
;;; button


(define (compute-impulse-weapon-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-impulse-weapon-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed impulse weapon button

(define (make-impulse-weapon-button screen::Screen)
  (RadioButton screen "ImpulseWeaponButton"
               (compute-impulse-weapon-button-origin screen)
               (compute-impulse-weapon-button-size screen)))


;;; (compute-malware-weapon-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the malware weapon
;;; button


(define (compute-malware-weapon-button-origin screen::Screen)
  (let ((weapons-palette-size (compute-weapons-palette-size screen))
        (weapon-button-size (compute-cannon-weapon-button-size screen)))
    (Vector2f (- (/ (*:getX weapons-palette-size) 2.0)
                 (/ (*:getX weapon-button-size) 2.0))
              (- (/ (* 5 (*:getY weapons-palette-size)) 8.0)
                 (/ (*:getY weapon-button-size) 2.0)))))


;;; (compute-malware-weapon-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the malware weapon
;;; button


(define (compute-malware-weapon-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-malware-weapon-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed malware weapon button

(define (make-malware-weapon-button screen::Screen)
  (RadioButton screen "MalwareWeaponButton"
               (compute-malware-weapon-button-origin screen)
               (compute-malware-weapon-button-size screen)))


;;; (compute-bots-weapon-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the bots weapon
;;; button


(define (compute-bots-weapon-button-origin screen::Screen)
  (let ((weapons-palette-size (compute-weapons-palette-size screen))
        (weapon-button-size (compute-cannon-weapon-button-size screen)))
    (Vector2f (- (/ (*:getX weapons-palette-size) 2.0)
                 (/ (*:getX weapon-button-size) 2.0))
              (- (/ (* 7 (*:getY weapons-palette-size)) 8.0)
                 (/ (*:getY weapon-button-size) 2.0)))))


;;; (compute-bots-weapon-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the bots weapon
;;; button


(define (compute-bots-weapon-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-bots-weapon-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed bots weapon button

(define (make-bots-weapon-button screen::Screen)
  (RadioButton screen "BotsWeaponButton"
               (compute-bots-weapon-button-origin screen)
               (compute-bots-weapon-button-size screen)))




;;; =====================================================================
;;; the armor palette
;;; =====================================================================

;;; CLASS ArmorButtonGroup
;;; ---------------------------------------------------------------------
;;; a RadioButtonGroup subclass used to present armor options to
;;; players

(defclass ArmorButtonGroup (RadioButtonGroup)
  (slots:
   (app-state init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (let ((button-id (*:getUID value))
          (state::CharacterCreatorAppState app-state))
      (cond
       ((equal? "AbsorbArmorButton" button-id)(set-current-armor state 'absorb-armor))
       (else (format #t "~%Unknown armor selected")))))))

;;; (compute-armor-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the armor palette, taking
;;; into account the dimensions of the screen

(define (compute-armor-palette-origin screen::Screen)
  (let ((weapons-palette-origin (compute-weapons-palette-origin screen))
        (armor-palette-size (compute-armor-palette-size screen))
        (screen-width (*:getWidth screen)))
    (Vector2f (- screen-width (*:getX armor-palette-size) 8)
              (*:getY weapons-palette-origin))))


;;; (compute-armor-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the armor palette, taking
;;; into account the dimensions of the screen

(define (compute-armor-palette-size screen::Screen)
  (let ((weapons-palette-size (compute-weapons-palette-size screen)))
    (Vector2f (*:getX weapons-palette-size)
              (*:getY weapons-palette-size))))


;;; (make-armor-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-contructed armor palette

(define (make-armor-palette screen::Screen)
  (Window screen "ArmorPalette"
          (compute-armor-palette-origin screen)
          (compute-armor-palette-size screen)))

;;; (compute-absorb-armor-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button

(define (compute-absorb-armor-button-origin screen::Screen)
  (let ((armor-palette-size (compute-armor-palette-size screen))
        (armor-button-size (compute-absorb-armor-button-size screen)))
    (Vector2f (- (/ (*:getX armor-palette-size) 2.0)
                 (/ (*:getX armor-button-size) 2.0))
              (- (/ (* 1 (*:getY armor-palette-size)) 8.0)
                 (/ (*:getY armor-button-size) 2.0)))))


;;; (compute-absorb-armor-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-absorb-armor-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-absorb-armor-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-absorb-armor-button screen::Screen)
  (RadioButton screen "AbsorbArmorButton"
               (compute-absorb-armor-button-origin screen)
               (compute-absorb-armor-button-size screen)))


;;; (compute-regen-armor-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-regen-armor-button-origin screen::Screen)
  (let ((armor-palette-size (compute-armor-palette-size screen))
        (armor-button-size (compute-absorb-armor-button-size screen)))
    (Vector2f (- (/ (*:getX armor-palette-size) 2.0)
                 (/ (*:getX armor-button-size) 2.0))
              (- (/ (* 3 (*:getY armor-palette-size)) 8.0)
                 (/ (*:getY armor-button-size) 2.0)))))


;;; (compute-regen-armor-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-regen-armor-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-regen-armor-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-regen-armor-button screen::Screen)
  (RadioButton screen "RegenArmorButton"
               (compute-regen-armor-button-origin screen)
               (compute-regen-armor-button-size screen)))


;;; (compute-power-armor-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-power-armor-button-origin screen::Screen)
  (let ((armor-palette-size (compute-armor-palette-size screen))
        (armor-button-size (compute-absorb-armor-button-size screen)))
    (Vector2f (- (/ (*:getX armor-palette-size) 2.0)
                 (/ (*:getX armor-button-size) 2.0))
              (- (/ (* 5 (*:getY armor-palette-size)) 8.0)
                 (/ (*:getY armor-button-size) 2.0)))))


;;; (compute-power-armor-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-power-armor-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-power-armor-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-power-armor-button screen::Screen)
  (RadioButton screen "PowerArmorButton"
               (compute-power-armor-button-origin screen)
               (compute-power-armor-button-size screen)))


;;; (compute-energy-armor-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-energy-armor-button-origin screen::Screen)
  (let ((armor-palette-size (compute-armor-palette-size screen))
        (armor-button-size (compute-absorb-armor-button-size screen)))
    (Vector2f (- (/ (*:getX armor-palette-size) 2.0)
                 (/ (*:getX armor-button-size) 2.0))
              (- (/ (* 7 (*:getY armor-palette-size)) 8.0)
                 (/ (*:getY armor-button-size) 2.0)))))


;;; (compute-energy-armor-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-energy-armor-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-energy-armor-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-energy-armor-button screen::Screen)
  (RadioButton screen "EnergyArmorButton"
               (compute-energy-armor-button-origin screen)
               (compute-energy-armor-button-size screen)))




;;; =====================================================================
;;; the augment palette
;;; =====================================================================

;;; CLASS AugmentsButtonGroup
;;; ---------------------------------------------------------------------
;;; a RadioButtonGroup subclass used to present augments options to
;;; players

(defclass AugmentsButtonGroup (RadioButtonGroup)
  (slots:
   (app-state init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (let ((button-id (*:getUID value))
          (state::CharacterCreatorAppState app-state))
      (cond
       ((equal? "AbsorbAugmentsButton" button-id)(set-current-augment state 'force-augment))
       (else (format #t "~%Unknown augment selected")))))))

;;; (compute-augment-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the augment palette, taking
;;; into account the dimensions of the screen

(define (compute-augment-palette-origin screen::Screen)
  (let ((faction-palette-origin (compute-faction-palette-origin screen))
        (faction-palette-size (compute-faction-palette-size screen))
        (screen-width (*:getWidth screen)))
    (Vector2f (- screen-width (+ 616 8))
              (*:getY faction-palette-origin))))


;;; (compute-augment-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the augment palette, taking
;;; into account the dimensions of the screen

(define (compute-augment-palette-size screen::Screen)
  (let ((faction-palette-origin (compute-faction-palette-origin screen))
        (faction-palette-size (compute-faction-palette-size screen))
        (screen-width (*:getWidth screen)))
    (Vector2f 616
              (*:getY faction-palette-size))))


;;; (make-augment-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-contructed augment palette

(define (make-augment-palette screen::Screen)
  (Window screen "AugmentPalette"
          (compute-augment-palette-origin screen)
          (compute-augment-palette-size screen)))


;;; (compute-force-augment-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-force-augment-button-origin screen::Screen)
  (let ((augment-palette-size (compute-augment-palette-size screen))
        (force-augment-button-size (compute-force-augment-button-size screen)))
    (Vector2f 16
              (- (/ (*:getY augment-palette-size) 2.0)
                 (/ (*:getY force-augment-button-size) 2.0)))))


;;; (compute-force-augment-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-force-augment-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-force-augment-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-force-augment-button screen::Screen)
  (RadioButton screen "ForceAugmentButton"
               (compute-force-augment-button-origin screen)
               (compute-force-augment-button-size screen)))


;;; (compute-optics-augment-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-optics-augment-button-origin screen::Screen)
  (let ((augments-palette-size (compute-augment-palette-size screen))
        (optics-augment-button-size (compute-optics-augment-button-size screen)))
    (Vector2f 176
              (- (/ (*:getY augments-palette-size) 2.0)
                 (/ (*:getY optics-augment-button-size) 2.0)))))


;;; (compute-optics-augment-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-optics-augment-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-optics-augment-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-optics-augment-button screen::Screen)
  (RadioButton screen "OpticsAugmentButton"
               (compute-optics-augment-button-origin screen)
               (compute-optics-augment-button-size screen)))


;;; (compute-portals-augment-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-portals-augment-button-origin screen::Screen)
  (let ((augments-palette-size (compute-augment-palette-size screen))
        (portals-augment-button-size (compute-portals-augment-button-size screen)))
    (Vector2f 320
              (- (/ (*:getY augments-palette-size) 2.0)
                 (/ (*:getY portals-augment-button-size) 2.0)))))


;;; (compute-portals-augment-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-portals-augment-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-portals-augment-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-portals-augment-button screen::Screen)
  (RadioButton screen "PortalsAugmentButton"
               (compute-portals-augment-button-origin screen)
               (compute-portals-augment-button-size screen)))


;;; (compute-turrets-augment-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-turrets-augment-button-origin screen::Screen)
  (let ((augments-palette-size (compute-augment-palette-size screen))
        (turrets-augment-button-size (compute-turrets-augment-button-size screen)))
    (Vector2f 464
              (- (/ (*:getY augments-palette-size) 2.0)
                 (/ (*:getY turrets-augment-button-size) 2.0)))))


;;; (compute-turrets-augment-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-turrets-augment-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-turrets-augment-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-turrets-augment-button screen::Screen)
  (RadioButton screen "TurretsAugmentButton"
               (compute-turrets-augment-button-origin screen)
               (compute-turrets-augment-button-size screen)))





;;; =====================================================================
;;; the AppState Class
;;; =====================================================================

;;; CLASS CharacterRotator (AbstractControl)
;;; ---------------------------------------------------------------------
;;; a control that slowly rotates a character that is under construction

(defclass CharacterRotator (AbstractControl)
  (slots:
   (xrate type: float init-form: 0.0 getter: getXRate setter: setXRate)
   (yrate type: float init-form: 0.0 getter: getYRate setter: setYRate)
   (zrate type: float init-form: 0.0 getter: getZRate setter: setZRate))
  (methods:
   ((*init* xr yr zr)(begin (set! xrate xr)
                            (set! yrate yr)
                            (set! zrate zr)))
   ((controlUpdate tpf)
    (when (*:getSpatial (this))
      (*:rotate (*:getSpatial (this))
                (* tpf xrate)
                (* tpf yrate)
                (* tpf zrate))))
   ;; dummy update method to make Java happy
   ((controlRender renderManager viewPort) #!void)))

;;; (make-character-creator-rotator)
;;; ---------------------------------------------------------------------
;;; returns a control that slowly rotates the character cube that is
;;; under construction
(define (make-character-creator-rotator)
  (CharacterRotator 0.1 0.2 0.0))

;;; CLASS CharacterCreatorAppState
;;; ---------------------------------------------------------------------
;;; the AppState class that constructs and manages the character
;;; creator scene in the Fabric client

(defclass CharacterCreatorAppState (AbstractAppState)
  (slots:
   (current-character init-form: #f getter: getCurrentCharacter setter: setCurrentCharacter)
   (current-faction init-form: #f getter: getCurrentFaction setter: setCurrentFaction)
   (character-nameplate::TLabel init-form: #!null getter: getCharacterNameplate)
   (faction-nameplate::TLabel init-form: #!null getter: getFactionNameplate)
   (app::SimpleApplication init-form: #!null getter: getApp setter: setApp)
   (state-manager::AppStateManager init-form: #!null getter: getStateManager setter: setStateManager))
  (methods:
   ((initialize mgr::AppStateManager client::SimpleApplication)
    (*:setApp (this) client)
    (let* ((screen (Screen client))
           (gui-node (*:getGuiNode client))
           (root-node (*:getRootNode (as SimpleApplication app)))
           (align BitmapFont:Align)
           (valign BitmapFont:VAlign)
           (sky (make-sky app))
           (cnameplate::TLabel (make-character-nameplate screen))
           (fnameplate::TLabel (make-faction-nameplate screen))
           (faction-palette (make-faction-palette screen))
           (faction-group (FactionButtonGroup screen "FactionGroup"))
           (highlight-color (ColorRGBA 1.0 1.0 0.0 1.0))
           (caretaker-button (make-caretaker-button screen))
           (abjurer-button (make-abjurer-button screen))
           (rogue-button (make-rogue-button screen))
           (character (make-player-character))
           (char-cube (get-property character 'cube: default: #f))
           (name-palette::Window (make-name-palette (this) screen))
           (armor-palette (make-armor-palette screen))
           (armor-group (ArmorButtonGroup screen "ArmorGroup"))
           (absorb-armor-button (make-absorb-armor-button screen))
           (regen-armor-button (make-regen-armor-button screen))
           (power-armor-button (make-power-armor-button screen))
           (energy-armor-button (make-energy-armor-button screen))
           (weapons-palette (make-weapons-palette screen))
           (weapons-group (WeaponsButtonGroup screen "WeaponsGroup"))
           (cannon-weapon-button (make-cannon-weapon-button screen))
           (impulse-weapon-button (make-impulse-weapon-button screen))
           (malware-weapon-button (make-malware-weapon-button screen))
           (bots-weapon-button (make-bots-weapon-button screen))
           (augments-palette (make-augment-palette screen))
           (force-augment-button (make-force-augment-button screen))
           (optics-augment-button (make-optics-augment-button screen))
           (portals-augment-button (make-portals-augment-button screen))
           (turrets-augment-button (make-turrets-augment-button screen))
           (augments-group (AugmentsButtonGroup screen "AugmentsGroup")))
      ;; --------------------
      ;; init the faction buttons
      ;; --------------------
      (set-app-state! faction-group (this))
      ;; --------------------
      ;; add the sky to the scene
      ;; --------------------
      (*:attachChild root-node sky)
      ;; --------------------
      ;; add the character model
      ;; --------------------
      (if char-cube
          (let ((rotator::CharacterRotator (make-character-creator-rotator)))
            (*:attachChild root-node (as Node char-cube))
            (*:setLocalTranslation (as Node char-cube) 0.0 0.0 -4.0)
            (*:setCurrentCharacter (this) character)
            (*:addControl (as Node char-cube) rotator)))
      ;; --------------------
      ;; add the character-nameplate
      ;; --------------------
      (set! character-nameplate cnameplate)
      (*:setText cnameplate "")
      (*:setTextAlign cnameplate align:Left)
      (*:setFont cnameplate "Interface/Fonts/Laconic30.fnt")
      (*:setFontSize cnameplate 30)
      (*:setFontColor cnameplate ColorRGBA:Green)
      (*:addElement screen cnameplate)
      ;; --------------------
      ;; add the faction-nameplate
      ;; --------------------
      (set! faction-nameplate fnameplate)
      (*:setText fnameplate "")
      (*:setTextAlign fnameplate align:Left)
      (*:setFont fnameplate "Interface/Fonts/Laconic30.fnt")
      (*:setFontSize fnameplate 30)
      (*:setFontColor fnameplate ColorRGBA:Green)
      (*:addElement screen fnameplate)
      ;; --------------------
      ;; faction palette
      ;; --------------------
      (*:setWindowTitle faction-palette "Choose a Faction:")
      ;; caretaker button
      (*:setButtonIcon caretaker-button 128 128 "Interface/caretakers-icon128.png")
      (*:setButtonPressedInfo caretaker-button "Interface/caretakers-icon-lit128.png"
                              (ColorRGBA 0.0 1.0 0.0 1.0))
      (*:setText caretaker-button "Caretakers")
      (*:setTextAlign caretaker-button align:Center)
      (*:setTextVAlign caretaker-button valign:Bottom)
      (*:setFontSize caretaker-button 20)
      (*:addButton faction-group caretaker-button)
      ;; abjurer button
      (*:setButtonIcon abjurer-button 128 128 "Interface/abjurers-icon128.png")
      (*:setButtonPressedInfo abjurer-button "Interface/abjurers-icon-lit128.png"
                              (ColorRGBA 1.0 0.0 0.0 1.0))
      (*:setText abjurer-button "Abjurers")
      (*:setTextAlign abjurer-button align:Center)
      (*:setTextVAlign abjurer-button valign:Bottom)
      (*:setFontSize abjurer-button 20)
      (*:addButton faction-group abjurer-button)
      ;; rogue button
      (*:setButtonIcon rogue-button 128 128 "Interface/rogues-icon128.png")
      (*:setButtonPressedInfo rogue-button "Interface/rogues-icon-lit128.png"
                              (ColorRGBA 0.0 0.75 1.0 1.0))
      (*:setText rogue-button "Rogues")
      (*:setTextAlign rogue-button align:Center)
      (*:setTextVAlign rogue-button valign:Bottom)
      (*:setFontSize rogue-button 20)
      (*:addButton faction-group rogue-button)
      (*:addChild faction-palette caretaker-button)
      (*:addChild faction-palette abjurer-button)
      (*:addChild faction-palette rogue-button)
      (*:addElement screen faction-palette)
      ;; --------------------
      ;; name palette
      ;; --------------------
      (*:setWindowTitle name-palette "Choose a Name:")
      (*:addElement screen name-palette)
      ;; --------------------
      ;; armor palette
      ;; --------------------
      (*:setWindowTitle armor-palette "Choose Armor:")
      ;; absorb armor button
      (*:setButtonIcon absorb-armor-button 128 128 "Interface/absorb-armor-icon128.png")
      (*:setButtonPressedInfo absorb-armor-button "Interface/absorb-armor-icon128.png"
                              (ColorRGBA 1.0 1.0 0.0 1.0))
      (*:setText absorb-armor-button "Absorb")
      (*:setTextAlign absorb-armor-button align:Center)
      (*:setTextVAlign absorb-armor-button valign:Bottom)
      (*:setFontSize absorb-armor-button 20)
      (*:addButton armor-group absorb-armor-button)
      (*:addChild armor-palette absorb-armor-button)
      ;; regen armor button
      (*:setButtonIcon regen-armor-button 128 128 "Interface/regen-armor-icon128.png")
      (*:setButtonPressedInfo regen-armor-button "Interface/regen-armor-icon128.png"
                              (ColorRGBA 0.0 1.0 0.0 1.0))
      (*:setText regen-armor-button "Regenerate")
      (*:setTextAlign regen-armor-button align:Center)
      (*:setTextVAlign regen-armor-button valign:Bottom)
      (*:setFontSize regen-armor-button 20)
      (*:addButton armor-group regen-armor-button)
      (*:addChild armor-palette regen-armor-button)
      ;; power armor button
      (*:setButtonIcon power-armor-button 128 128 "Interface/power-armor-icon128.png")
      (*:setButtonPressedInfo power-armor-button "Interface/power-armor-icon128.png"
                              (ColorRGBA 1.0 0.5 0.0 1.0))
      (*:setText power-armor-button "Power")
      (*:setTextAlign power-armor-button align:Center)
      (*:setTextVAlign power-armor-button valign:Bottom)
      (*:setFontSize power-armor-button 20)
      (*:addButton armor-group power-armor-button)
      (*:addChild armor-palette power-armor-button)
      ;; energy armor button
      (*:setButtonIcon energy-armor-button 128 128 "Interface/energy-armor-icon128.png")
      (*:setButtonPressedInfo energy-armor-button "Interface/energy-armor-icon128.png"
                              (ColorRGBA 0.0 6.0 1.0 1.0))
      (*:setText energy-armor-button "Energy")
      (*:setTextAlign energy-armor-button align:Center)
      (*:setTextVAlign energy-armor-button valign:Bottom)
      (*:setFontSize energy-armor-button 20)
      (*:addButton armor-group energy-armor-button)
      (*:addChild armor-palette energy-armor-button)
      ;; add palette to screen
      (*:addElement screen armor-palette)
      ;; --------------------
      ;; weapons palette
      ;; --------------------
      (*:setWindowTitle weapons-palette "Choose Weapons:")
      ;; cannon weapon button
      (*:setButtonIcon cannon-weapon-button 128 128 "Interface/cannon-weapon-icon128.png")
      (*:setButtonPressedInfo cannon-weapon-button "Interface/cannon-weapon-icon128.png"
                              (ColorRGBA 1.0 1.0 0.0 1.0))
      (*:setText cannon-weapon-button "Cannon")
      (*:setTextAlign cannon-weapon-button align:Center)
      (*:setTextVAlign cannon-weapon-button valign:Bottom)
      (*:setFontSize cannon-weapon-button 20)
      (*:addButton weapons-group cannon-weapon-button)
      (*:addChild weapons-palette cannon-weapon-button)
      ;; impulse weapon button
      (*:setButtonIcon impulse-weapon-button 128 128 "Interface/impulse-weapon-icon128.png")
      (*:setButtonPressedInfo impulse-weapon-button "Interface/impulse-weapon-icon128.png"
                              (ColorRGBA 0.0 1.0 0.0 1.0))
      (*:setText impulse-weapon-button "Impulse")
      (*:setTextAlign impulse-weapon-button align:Center)
      (*:setTextVAlign impulse-weapon-button valign:Bottom)
      (*:setFontSize impulse-weapon-button 20)
      (*:addButton weapons-group impulse-weapon-button)
      (*:addChild weapons-palette impulse-weapon-button)
      ;; malware weapon button
      (*:setButtonIcon malware-weapon-button 128 128 "Interface/malware-weapon-icon128.png")
      (*:setButtonPressedInfo malware-weapon-button "Interface/malware-weapon-icon128.png"
                              (ColorRGBA 1.0 0.5 0.0 1.0))
      (*:setText malware-weapon-button "Malware")
      (*:setTextAlign malware-weapon-button align:Center)
      (*:setTextVAlign malware-weapon-button valign:Bottom)
      (*:setFontSize malware-weapon-button 20)
      (*:addButton weapons-group malware-weapon-button)
      (*:addChild weapons-palette malware-weapon-button)
      ;; bots weapon button
      (*:setButtonIcon bots-weapon-button 128 128 "Interface/bots-weapon-icon128.png")
      (*:setButtonPressedInfo bots-weapon-button "Interface/bots-weapon-icon128.png"
                              (ColorRGBA 0.0 6.0 1.0 1.0))
      (*:setText bots-weapon-button "Bots")
      (*:setTextAlign bots-weapon-button align:Center)
      (*:setTextVAlign bots-weapon-button valign:Bottom)
      (*:setFontSize bots-weapon-button 20)
      (*:addButton weapons-group bots-weapon-button)
      (*:addChild weapons-palette bots-weapon-button)
      (*:addElement screen weapons-palette)
      ;; --------------------
      ;; augment palette
      ;; --------------------
      (*:setWindowTitle augments-palette "Choose Augments:")
      ;; force augment button
      (*:setButtonIcon force-augment-button 128 128 "Interface/force-augment-icon128.png")
      (*:setButtonPressedInfo force-augment-button "Interface/force-augment-icon128.png"
                              (ColorRGBA 1.0 1.0 0.0 1.0))
      (*:setText force-augment-button "Force Fields")
      (*:setTextAlign force-augment-button align:Center)
      (*:setTextVAlign force-augment-button valign:Bottom)
      (*:setFontSize force-augment-button 20)
      (*:addButton augments-group force-augment-button)
      (*:addChild augments-palette force-augment-button)
      ;; optics augment button
      (*:setButtonIcon optics-augment-button 128 128 "Interface/optics-augment-icon128.png")
      (*:setButtonPressedInfo optics-augment-button "Interface/optics-augment-icon128.png"
                              (ColorRGBA 0.0 1.0 0.0 1.0))
      (*:setText optics-augment-button "Optics")
      (*:setTextAlign optics-augment-button align:Center)
      (*:setTextVAlign optics-augment-button valign:Bottom)
      (*:setFontSize optics-augment-button 20)
      (*:addButton augments-group optics-augment-button)
      (*:addChild augments-palette optics-augment-button)
      ;; portals augment button
      (*:setButtonIcon portals-augment-button 128 128 "Interface/portals-augment-icon128.png")
      (*:setButtonPressedInfo portals-augment-button "Interface/portals-augment-icon128.png"
                              (ColorRGBA 1.0 0.5 0.0 1.0))
      (*:setText portals-augment-button "Portals")
      (*:setTextAlign portals-augment-button align:Center)
      (*:setTextVAlign portals-augment-button valign:Bottom)
      (*:setFontSize portals-augment-button 20)
      (*:addButton augments-group portals-augment-button)
      (*:addChild augments-palette portals-augment-button)
      ;; turrets augment button
      (*:setButtonIcon turrets-augment-button 128 128 "Interface/turrets-augment-icon128.png")
      (*:setButtonPressedInfo turrets-augment-button "Interface/turrets-augment-icon128.png"
                              (ColorRGBA 0.0 6.0 1.0 1.0))
      (*:setText turrets-augment-button "Turrets")
      (*:setTextAlign turrets-augment-button align:Center)
      (*:setTextVAlign turrets-augment-button valign:Bottom)
      (*:setFontSize turrets-augment-button 20)
      (*:addButton augments-group turrets-augment-button)
      (*:addChild augments-palette turrets-augment-button)
      (*:addElement screen augments-palette)
      ;; --------------------
      ;; add the gui to the scene
      ;; --------------------
      (*:addControl gui-node screen)))))


;;; (update-state-for-faction state::CharacterCreatorAppState faction)
;;; ---------------------------------------------------------------------
;;; updates the display state of the character creator's user interface
;;; in response to any changes made by the user or the game

(define (update-state-for-faction state::CharacterCreatorAppState faction)
  (let* ((character-entity (*:getCurrentCharacter state)))
    (if (member faction '(caretakers rogues abjurers))
        (let ((char-cube (get-property character-entity 'cube: default: #f)))
          (if char-cube
              (case faction
                ((caretakers)(let ((fnameplate::TLabel (*:getFactionNameplate state)))
                               (set-player-character-cube-color! char-cube (caretakers-character-color))
                               (*:setText fnameplate "Faction: Caretakers")))
                ((rogues)(let ((fnameplate::TLabel (*:getFactionNameplate state)))
                           (set-player-character-cube-color! char-cube (rogues-character-color))
                           (*:setText fnameplate "Faction: Rogues")))
                ((abjurers)(let ((fnameplate::TLabel (*:getFactionNameplate state)))
                             (set-player-character-cube-color! char-cube (abjurers-character-color))
                             (*:setText fnameplate "Faction: Abjurers"))))
              (error "update-state-for-faction: missing character cube ")))
        (error "set-current-faction: unrecognized faction: " faction))))


;;; (set-current-faction state::CharacterCreatorAppState faction)
;;; ---------------------------------------------------------------------
;;; updates the currently-selected player faction in the character
;;; creator in response to a user selection

(define (set-current-faction state::CharacterCreatorAppState faction)
  (if (member faction '(caretakers rogues abjurers))
      (begin (*:setCurrentFaction state faction)
             (update-state-for-faction state faction))
      (error "set-current-faction: unrecognized faction: " faction)))


;;; (set-current-armor state::CharacterCreatorAppState armor)
;;; ---------------------------------------------------------------------
;;; updates the currently-selected player armor in the character
;;; creator in response to a user selection

(define (set-current-armor state::CharacterCreatorAppState armor)
  (format #t "~%chose armor: ~S" armor))


;;; (set-current-weapon state::CharacterCreatorAppState weapon)
;;; ---------------------------------------------------------------------
;;; updates the currently-selected player weapon in the character
;;; creator in response to a user selection

(define (set-current-weapon state::CharacterCreatorAppState weapon)
  (format #t "~%chose weapon: ~S" weapon))


;;; (set-current-augment state::CharacterCreatorAppState augment)
;;; ---------------------------------------------------------------------
;;; updates the currently-selected player augment in the character
;;; creator in response to a user selection

(define (set-current-augment state::CharacterCreatorAppState augment)
  (format #t "~%chose augment: ~S" augment))

(define (notify-name-selection-changed app-state index value)
  (format #t "~%name selection changed; state: ~S, index: ~S, value: ~S"
          app-state index value))
