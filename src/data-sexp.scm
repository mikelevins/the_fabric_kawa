;;;; ***********************************************************************
;;;;
;;;; Name:          data-sexp.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       convert game data to and from s-expressions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 character->sexp
 character->sexp-string
 name->sexp
 name->sexp-string
 rectangle->sexp
 rectangle->sexp-string
 sexp->character
 sexp->name
 sexp->object
 sexp->user
 sexp-string->character
 sexp-string->name
 sexp-string->rectangle
 string->sexp
 user->sexp
 user->sexp-string
 )

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-lists)
(require model-rect)
(require model-character)
(require model-namegen)
(require model-user)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (rnrs hashtables))
(import (class gnu.lists Pair))
(import (class java.lang String))

;;; ---------------------------------------------------------------------
;;; conversions between s-expressions and strings
;;; ---------------------------------------------------------------------

(define conversion-table (make-parameter (make-eqv-hashtable)))

(define (register-converter! type-tag converter)
  (hashtable-set! (conversion-table) type-tag converter))

(define (sexp->string sexp)
  (format #f "~S" sexp))

(define (string->sexp str::String)
  (call-with-input-string str (lambda (in)(read in))))

(define (sexp->object sexp::Pair)
  (let* ((type-tag (car sexp))
         (converter (hashtable-ref (conversion-table) type-tag #f)))
    (if converter
        (converter sexp)
        (error "No reader defined for objects of type " type-tag))))

;;; ---------------------------------------------------------------------
;;; rectangles
;;; ---------------------------------------------------------------------

(define (rectangle->sexp rect)
  `(rectangle left: ,(get-left rect)
              top: ,(get-top rect)
              width: ,(get-width rect)
              height: ,(get-height rect)))

(define (rectangle->sexp-string rect)
  (sexp->string (rectangle->sexp rect)))

(define (%read-rectangle-sexp sexp)
  (let ((left (get-key sexp 'left: 0))
        (top (get-key sexp 'top: 0))
        (width (get-key sexp 'width: 0))
        (height (get-key sexp 'height: 0)))
    (make-rectangle left top width height)))

(define (sexp->rectangle sexp::Pair)
  (if (eq? 'rectangle (car sexp))
      (%read-rectangle-sexp sexp)
      (error "sexp->rectangle expected a list beginning with the symbol rectangle but found "
             sexp)))

(register-converter! 'rectangle sexp->rectangle)

(define (sexp-string->rectangle str::String)
  (sexp->rectangle (string->sexp str)))

;;; ---------------------------------------------------------------------
;;; FabricNames
;;; ---------------------------------------------------------------------

(define (name->sexp fname::FabricName)
  (let* ((data::int[] fname:data)
        (data-list (integer-array->list data)))
    `(FabricName data: ,data-list)))

(define (name->sexp-string fname::FabricName)
  (sexp->string (name->sexp fname)))

(define (%read-fabric-name-sexp sexp)
  (let* ((data-sexp (get-key sexp 'data: #f)))
    (if data-sexp
        (FabricName (apply int[] data-sexp))
        (blank-fabric-name))))

(define (sexp->name sexp::Pair)
  (if (eq? 'FabricName (car sexp))
      (%read-fabric-name-sexp sexp)
      (error "sexp->name expected a list beginning with the symbol FabricName but found "
             sexp)))

(register-converter! 'FabricName sexp->name)

(define (sexp-string->name str::String)
  (sexp->name (string->sexp str)))

;;; ---------------------------------------------------------------------
;;; FabricCharacters
;;; ---------------------------------------------------------------------

(define (character->sexp character::FabricCharacter)
  (let* ((name-sexp (name->sexp character:name))
         (faction-name character:faction)
         (weapon-name character:weapon)
         (armor-name character:armor)
         (augment-name character:augment))
    `(FabricCharacter name: ,name-sexp
                      faction: ,faction-name
                      weapon: ,weapon-name
                      armor: ,armor-name
                      augment: ,augment-name)))

(define (character->sexp-string character::FabricCharacter)
  (sexp->string (character->sexp character)))

(define (%read-fabric-character-sexp sexp)
  (let* ((name-sexp (get-key sexp 'name: #f))
         (fname (if name-sexp (sexp->name name-sexp) (blank-fabric-name)))
         (faction-sexp (get-key sexp 'faction: #f))
         (ffaction (if faction-sexp (string->symbol faction-sexp) #!null))
         (weapon-sexp (get-key sexp 'weapon: #f))
         (fweapon (if weapon-sexp (string->symbol weapon-sexp) #!null))
         (armor-sexp (get-key sexp 'armor: #f))
         (farmor (if armor-sexp (string->symbol armor-sexp) #!null))
         (augment-sexp (get-key sexp 'augment: #f))
         (faugment (if augment-sexp (string->symbol augment-sexp) #!null))
         (fchar (make-fabric-character fname)))
    (set! fchar:faction ffaction)
    (set! fchar:weapon fweapon)
    (set! fchar:armor farmor)
    (set! fchar:augment faugment)
    fchar))

(define (sexp->character sexp::Pair)
  (if (eq? 'FabricCharacter (car sexp))
      (%read-fabric-character-sexp sexp)
      (error "sexp->character expected a list beginning with the symbol FabricCharacter but found "
             sexp)))

(register-converter! 'FabricCharacter sexp->character)

(define (sexp-string->character str::String)
  (sexp->character (string->sexp str)))

;;; ---------------------------------------------------------------------
;;; FabricUsers
;;; ---------------------------------------------------------------------

(define (user->sexp user::FabricUser)
  (let* ((username user:username)
         (pwhash user:password-hash)
         (pwsalt (integer-array->list user:password-salt))
         (chars (map character->sexp user:characters)))
    `(FabricUser username: ,username
                 password-hash: ,pwhash
                 password-salt: ,pwsalt
                 characters: ,chars)))

(define (user->sexp-string user::FabricUser)
  (sexp->string (user->sexp user)))

(define (%read-fabric-user-sexp sexp)
  (let* ((username-sexp (get-key sexp 'username: #f))
         (username (or username-sexp #!null))
         (password-hash-sexp (get-key sexp 'password-hash: #f))
         (pwhash (or password-hash-sexp #!null))
         (password-salt-sexp (get-key sexp 'password-salt: #f))
         (pwsalt (if password-salt-sexp
                     (if (eqv? #!null password-salt-sexp)
                         #!null
                         (apply int[] password-salt-sexp))
                     #!null))
         (characters-sexp (get-key sexp 'characters: #f))
         (characters (if characters-sexp
                         (map sexp->object characters-sexp)
                         '()))
         (user (make-fabric-user username: username
                                 password-hash: pwhash
                                 password-salt: pwsalt)))
    (set! user:characters (map (lambda (x) x) characters))
    user))

(define (sexp->user sexp::Pair)
  (if (eq? 'FabricUser (car sexp))
      (%read-fabric-user-sexp sexp)
      (error "sexp->user expected a list beginning with the symbol FabricUser but found "
             sexp)))

(register-converter! 'FabricUser sexp->user)


