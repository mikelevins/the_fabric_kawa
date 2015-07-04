;;; ***********************************************************************
;;;;
;;;; Name:          syntax-classes.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       conveniences for defining Java classes and interfaces
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading syntax-classes.cm")

(module-export
 defclass
 definterface)

(require 'list-lib)
(require "util-lists.scm")

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this file implements macros that extend Kawa with more compact and
;;; convenient syntax for defining Java classes and interfaces

;;; MACRO (definterface ifname supers method-prototype...)
;;; ---------------------------------------------------------------------
;;; define a new Java interfacename _ifname_. Each _method-prototype_
;;; becomes a new abstract method defined by the interface. The
;;; new interface extends the interfaces listed in _supers_.

(define-syntax definterface
  (syntax-rules ()
    ((definterface ifname supers method-prototype ...)
     (define-simple-class ifname supers interface: #t
       (method-prototype  #!abstract) ...))))


;;; (%slotspec form)
;;; ---------------------------------------------------------------------
;;; returns a normalized slot description of the form
;;; (sname type: stype init-form: sinit)
;;; by parsing _form_, supplying default values for any
;;; missing elements of the slot description

(define (%slotspec form)
  (let* ((sname (car form))
         (inits (cdr form))
         (sinit (get-key inits init-form: #!null))
         (stype (get-key inits type: java.lang.Object)))
    (list sname type: stype init-form: sinit)))


;;; (%slotaccessors form)
;;; ---------------------------------------------------------------------
;;; returns a list containing the slot-getter and the slot-setter from
;;; form, if any. The getter and setter are returned only if present
;;; in _form_. If neither is present in _form_ then %slotaccessors
;;; returns '()

(define (%slotaccessors form)
  (let* ((sname (car form))
         (inits (cdr form))
         (sgetter-form (get-key inits getter: #f))
         (sgetter (if sgetter-form
                      `((,sgetter-form) ,sname)
                      #f))
         (ssetter-form (get-key inits setter: #f))
         (svar (gentemp))
         (ssetter (if ssetter-form
                      `((,ssetter-form ,svar) (set! ,sname ,svar))
                      #f)))
    (filter (lambda (x) x)
            (list sgetter ssetter))))


;;; MACRO (defclass classname superclasses . class-body)
;;; ---------------------------------------------------------------------
;;; defines a new Java class using Kawa's define-simple-class form.
;;; defclass provides a more compact and readable syntax for defining
;;; classes, including syntax for automatically generating getter and
;;; setter methods for slots, and for annotating classes, slots, and
;;; methods. See numerous defclass forms in the Fabric sources for
;;; examples of use.

(defmacro defclass (classname superclasses . class-body)
  (let* ((annotations-clause (assoc annotations: class-body))
         (annotation-forms (if annotations-clause
                               (cdr annotations-clause)
                               #f))
         (slots-clause (assoc slots: class-body))
         (slot-specs (if slots-clause
                         (map %slotspec (cdr slots-clause))
                         '()))
         (slot-accessors (if slots-clause
                             (apply append (map %slotaccessors (cdr slots-clause)))
                             '()))
         (methods-clause (assoc methods: class-body))
         (method-forms (if methods-clause
                           (cdr methods-clause)
                           '()))
         (init-clause (assoc init: class-body))
         (init-forms (if init-clause
                         (cdr init-clause)
                         (list #f))))
    (if annotation-forms
        `(define-simple-class ,classname ,superclasses (,@annotation-forms)
           ,@slot-specs
           ,@slot-accessors
           ,@method-forms
           (init: (begin ,@init-forms)))
        `(define-simple-class ,classname ,superclasses
           ,@slot-specs
           ,@slot-accessors
           ,@method-forms
           (init: (begin ,@init-forms))))))
