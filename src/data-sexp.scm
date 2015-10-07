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
 define-serialization
 get-serializer
 object->s-expression
 s-expression->object)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-lists)
(require data-config)
(require model-rect)
(require model-character)
(require model-namegen)
(require model-user)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (srfi :69 basic-hash-tables))
(import (class gnu.lists Pair))
(import (class java.lang Class String))

;;; ---------------------------------------------------------------------
;;; conversions between s-expressions and strings
;;; ---------------------------------------------------------------------

(define serializer-table (make-parameter (make-hash-table equal?)))
(define deserializer-table (make-parameter (make-hash-table equal?)))

(define (define-serialization a-class::Class reader writer)
  (let ((class-name (*:getName a-class)))
    (hash-table-set! (serializer-table) class-name writer)
    (hash-table-set! (deserializer-table) class-name reader)))

(define (get-serializer obj)
  (let* ((the-class (*:getClass obj))
         (class-name (*:getName the-class))
         (serializer (hash-table-ref/default (serializer-table) class-name #f)))
    (or serializer #f)))

(define (object->s-expression obj)
  (let* ((the-class (*:getClass obj))
         (class-name (*:getName the-class))
         (writer (get-serializer obj)))
    (if writer
        (writer obj)
        (error (format #f "No serializer defined for ~S" class-name)))))

(define (s-expression->object sexp)
  (if (pair? sexp)
      (let ((class-name (car sexp)))
        (if (string? class-name)
            (let ((reader (hash-table-ref/default (deserializer-table) class-name #f)))
              (if reader
                  (reader sexp)
                  (error (format #f "No serializer defined for ~S" class-name))))
            (error (format #f "s-expression is not a serialized object"))))
      (error (format #f "s-expression is not a serialized object"))))

;;; ---------------------------------------------------------------------
;;; configuration data
;;; ---------------------------------------------------------------------

(define (%write-client-configuration-sexp conf::ClientConfiguration)
  `("ClientConfiguration"
    version: ,conf:version
    host: ,conf:host
    port: ,conf:port
    default-username: ,conf:default-username))

(define (%read-client-configuration-sexp sexp)
  (let ((version (or (get-key sexp 'version: #f)
                     (error "Missing version number in client configuration")))
        (host (or (get-key sexp 'host: #f)
                  (error "Missing hostname in client configuration")))
        (port (or (get-key sexp 'port: #f)
                  (error "Missing port number in client configuration")))
        (uname (or (get-key sexp 'default-username: #f)
                   (error "Missing port number in client configuration"))))
    (make-client-configuration version host port uname)))

(define-serialization ClientConfiguration %read-client-configuration-sexp %write-client-configuration-sexp)


(define (%write-server-configuration-sexp conf::ServerConfiguration)
  `("ServerConfiguration"
    version: ,conf:version
    port: ,conf:port))

(define (%read-server-configuration-sexp sexp)
  (let ((version (or (get-key sexp 'version: #f)
                     (error "Missing version number in server configuration")))
        (port (or (get-key sexp 'port: #f)
                  (error "Missing port number in server configuration"))))
    (make-server-configuration version port)))

(define-serialization ServerConfiguration %read-server-configuration-sexp %write-server-configuration-sexp)

;;; ---------------------------------------------------------------------
;;; rectangles
;;; ---------------------------------------------------------------------

(define (%write-rectangle-sexp rect)
  `("model$Mnrect$rectangle" left: ,(get-left rect)
    top: ,(get-top rect)
    width: ,(get-width rect)
    height: ,(get-height rect)))

(define (%read-rectangle-sexp sexp)
  (let ((left (get-key sexp 'left: 0))
        (top (get-key sexp 'top: 0))
        (width (get-key sexp 'width: 0))
        (height (get-key sexp 'height: 0)))
    (make-rectangle left top width height)))

(define-serialization <rectangle> %read-rectangle-sexp %write-rectangle-sexp)

;;; ---------------------------------------------------------------------
;;; FabricNames
;;; ---------------------------------------------------------------------


(define (%write-fabric-name-sexp fname::FabricName)
  (let* ((data::int[] fname:data)
         (data-list (integer-array->list data)))
    `("FabricName" data: ,data-list)))

(define (%read-fabric-name-sexp sexp)
  (let* ((data-sexp (get-key sexp 'data: #f)))
    (if data-sexp
        (FabricName (apply int[] data-sexp))
        (blank-fabric-name))))

(define-serialization FabricName %read-fabric-name-sexp %write-fabric-name-sexp)

;;; ---------------------------------------------------------------------
;;; FabricCharacters
;;; ---------------------------------------------------------------------

(define (%write-fabric-character-sexp character::FabricCharacter)
  (let* ((name-sexp (object->s-expression character:name))
         (faction-name character:faction)
         (weapon-name character:weapon)
         (armor-name character:armor)
         (augment-name character:augment))
    `("FabricCharacter" name: ,name-sexp
      faction: ,faction-name
      weapon: ,weapon-name
      armor: ,armor-name
      augment: ,augment-name)))

(define (%read-fabric-character-sexp sexp)
  (let* ((name-sexp (get-key sexp 'name: #f))
         (fname (if name-sexp (s-expression->object name-sexp) (blank-fabric-name)))
         (faction-sexp (get-key sexp 'faction: #f))
         (ffaction (or faction-sexp #!null))
         (weapon-sexp (get-key sexp 'weapon: #f))
         (fweapon (or weapon-sexp #!null))
         (armor-sexp (get-key sexp 'armor: #f))
         (farmor (or armor-sexp #!null))
         (augment-sexp (get-key sexp 'augment: #f))
         (faugment (or augment-sexp #!null))
         (fchar (make-fabric-character fname)))
    (set! fchar:faction ffaction)
    (set! fchar:weapon fweapon)
    (set! fchar:armor farmor)
    (set! fchar:augment faugment)
    fchar))

(define-serialization FabricCharacter %read-fabric-character-sexp %write-fabric-character-sexp)

;;; ---------------------------------------------------------------------
;;; FabricUsers
;;; ---------------------------------------------------------------------

(define (%write-fabric-user-sexp user::FabricUser)
  (let* ((username user:username)
         (pwhash user:password-hash)
         (pwsalt (byte-array->list user:password-salt))
         (chars (map object->s-expression user:characters)))
    `("FabricUser" username: ,username
      password-hash: ,pwhash
      password-salt: ,pwsalt
      characters: ,chars)))

(define (%read-fabric-user-sexp sexp)
  (let* ((username-sexp (get-key sexp 'username: #f))
         (username (or username-sexp #!null))
         (password-hash-sexp (get-key sexp 'password-hash: #f))
         (pwhash (or password-hash-sexp #!null))
         (password-salt-sexp (get-key sexp 'password-salt: #f))
         (pwsalt (if password-salt-sexp
                     (if (eqv? #!null password-salt-sexp)
                         #!null
                         (apply byte[] password-salt-sexp))
                     #!null))
         (characters-sexp (get-key sexp 'characters: #f))
         (characters (if characters-sexp
                         (map s-expression->object characters-sexp)
                         '()))
         (user (make-fabric-user username: username
                                 password-hash: pwhash
                                 password-salt: pwsalt)))
    (set! user:characters (map (lambda (x) x) characters))
    user))

(define-serialization FabricUser %read-fabric-user-sexp %write-fabric-user-sexp)

