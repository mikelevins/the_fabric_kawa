;;; ***********************************************************************
;;;;
;;;; Name:          syntax-classes.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       conveniences for defining Java classes and interfaces
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export definterface defclass)

(require 'list-lib)
(require "util-general.scm")
(require "util-lists.scm")

(define-syntax definterface
  (syntax-rules ()
    ((definterface ifname supers method-prototype ...)
     (define-simple-class ifname supers interface: #t
       (method-prototype  #!abstract) ...))))

(define (%slotspec form)
  (let* ((sname (car form))
         (inits (cdr form))
         (sinit (getf inits init-form: #!null))
         (stype (getf inits type: java.lang.Object)))
    (list sname type: stype init-form: sinit)))

(define (%slotaccessors form)
  (let* ((sname (car form))
         (inits (cdr form))
         (sgetter-form (getf inits getter: #f))
         (sgetter (if sgetter-form
                      `((,sgetter-form) ,sname)
                      #f))
         (ssetter-form (getf inits setter: #f))
         (svar (gentemp))
         (ssetter (if ssetter-form
                      `((,ssetter-form ,svar) (set! ,sname ,svar))
                      #f)))
    (filter (lambda (x) x)
            (list sgetter ssetter))))

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
