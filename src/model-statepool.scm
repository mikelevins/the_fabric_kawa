;;;; ***********************************************************************
;;;;
;;;; Name:          model-statepool.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       inventory management for AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 get-appstate)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; the Fabric swaps AppStates in and out, but JME3 AppStates, and
;;; especially UI elements, are not designed to be created and
;;; destroyed frequently. Especially troublesome is the tonegod UI
;;; elements are created with unique-per-session string IDs that
;;; cannot be easily recycled, and that cause crashes when reused on
;;; new UI elements.
;;;
;;; The solution is to create each AppState and its UI elements
;;; only once, and keep unused AppStates around in a pool for
;;; reuse, rather than destroying and recreating them. AppStatePool
;;; is the data structure that stores these states.
;;;
;;; to make this work, the Client app must obtain its AppStates
;;; by requesting them from the AppStatePool instead of creating
;;; them itself. The AppStatePool takes care of creating
;;; AppStates that have not repveiously been created, or
;;; furnishing existing ones, as the case may be.

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")
(require "gamestates.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; the AppStatePool
;;; ---------------------------------------------------------------------

(define *appstate-pool* '())

(define (%construct-login-appstate client)
  (let ((state (LoginGameState)))
    (*:setApp state client)
    state))

(define (%construct-createchar-appstate client)
  (let ((state (CreateCharacterGameState)))
    (*:setApp state client)
    state))

(define (%construct-pickchar-appstate client)
  (let ((state (PickCharacterGameState)))
    (*:setApp state client)
    state))

(define (%construct-play-appstate client)
  (let ((state (PlayGameState)))
    (*:setApp state client)
    state))

(define (%construct-transit-appstate client)
  (let ((state (TransitGameState)))
    (*:setApp state client)
    state))

(define (%construct-workshop-appstate client)
  (let ((state (WorkshopGameState)))
    (*:setApp state client)
    state))

(define *appstate-constructors*
  `((login . ,%construct-login-appstate)
    (create-character . ,%construct-createchar-appstate)
    (pick-character . ,%construct-pickchar-appstate)
    (play . ,%construct-play-appstate)
    (transit . ,%construct-transit-appstate)
    (workshop . ,%construct-workshop-appstate)))

(define (get-appstate-constructor state-name)
  (let ((entry (assq state-name *appstate-constructors*)))
    (if entry
        (cdr entry)
        #f)))

(define (validate-state-name state-name)
  (if (memq state-name (map car *appstate-constructors*))
      #t
      (error "Invalid AppState name" state-name)))

(define (%find-appstate-entry state-name)
  (assq state-name *appstate-pool*))

(define (%lookup-appstate state-name)
  (when (validate-state-name state-name)
    (let ((entry (%find-appstate-entry state-name)))
      (if entry
          (cdr entry)
          #f))))

(define (%construct-appstate client state-name)
  (let ((%construct (get-appstate-constructor state-name)))
    (if %construct
        (%construct client)
        (error "Constructor for AppState not found" state-name))))

(define (%add-appstate-entry! client state-name)
  (when (validate-state-name state-name)
    (let ((entry (%find-appstate-entry state-name)))
      (if entry
          (error "AppState already exists!" state-name)
          (let ((state (%construct-appstate client state-name)))
            (set! *appstate-pool*
                  (cons (cons state-name state)
                        *appstate-pool*))
            state-name)))))

(define (get-appstate client state-name)
  (let ((state (%lookup-appstate state-name)))
    (or state
        (begin (%add-appstate-entry! client state-name)
               (let ((state (%lookup-appstate state-name)))
                 (or state
                     (error "Error constructing AppState" state-name)))))))
