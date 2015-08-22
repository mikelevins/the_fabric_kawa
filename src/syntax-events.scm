;;; ***********************************************************************
;;;;
;;;; Name:          syntax-events.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       syntax definitions for event-handling
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 on-action
 on-analog
 route-keys)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file implements macros that simplify and clarify code that
;;; sets up event handling in the Fabric's JME3 classes.

;;; MACRO (on-analog (evt-name)(s -> expr) ...)
;;; ---------------------------------------------------------------------
;;; creates event handlers for the analog event named _evt-name_,
;;; where each event-type identifier _s_ is mapped to a handler
;;; _expr_, which may be any arbitrary Scheme expression. When an
;;; event _evt-name_ of type _s_ is signalled, _expr_ is executed.

(define-syntax on-analog
  (syntax-rules (->)
    ((on-analog (evt-name)
                (s -> expr) ...)
     (cond
      ((invoke evt-name 'equals s) expr) ...
      (#t #f)))))


;;; (on-action (evt-name)(s -> expr) ...)
;;; ---------------------------------------------------------------------
;;; creates event handlers for the action event named _evt-name_,
;;; where each event-type identifier _s_ is mapped to a handler
;;; _expr_, which may be any arbitrary Scheme expression. When an
;;; event _evt-name_ of type _s_ is signalled, _expr_ is executed.

(define-syntax on-action
  (syntax-rules (->)
    ((on-action (evt-name)
                (s -> expr) ...)
     (cond
      ((invoke evt-name 'equals s) expr) ...
      (#t #f)))))

;;; (route-keys (input-manager-object)(trigger-object -> event-name) ...)
;;; ---------------------------------------------------------------------
;;; creates event-routers for key events in which a key event
;;; _trigger-object_ is mapped to an event-name _event-name_. When
;;; _trigger-object_ is signalled, the macro signals event
;;; _event-name_, which is then handled by any appropriately-defined
;;; event-handlers.

(define-syntax route-keys
  (syntax-rules (->)
    ((route-keys (input-manager-object)
                 (trigger-object -> event-name)
                 ...)
     (begin
       (*:addMapping input-manager-object event-name trigger-object)
       ...))))

