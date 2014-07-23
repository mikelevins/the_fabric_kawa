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

(define-syntax definterface
  (syntax-rules ()
    ((definterface ifname supers method-prototype ...)
     (define-simple-class ifname supers interface: #t
       (method-prototype  #!abstract) ...))))


(define (%parse-defclass-superclasses supers-form)
  (if (or (null? supers-form)
          (and (list? supers-form)
               (every (lambda (f)(or (symbol? f)
                                     (instance? f java.lang.reflect.Type)))
                      supers-form)))
      supers-form
      (error (format #f "Invalid superclasses form: ~s" supers-form))))

(define (%parse-defclass-init body)
  (let ((init-clause (assoc init: body)))
    (if init-clause
        `(begin ,@(cdr init-clause))
        '(begin #f))))

(define (%parse-slot-description sdesc)
  (let* ((sname (car sdesc))
         (init-tail (member init-form: sdesc))
         (initval (if init-tail (cadr init-tail) #f))
         (type-tail (member type: sdesc))
         (typeval (if type-tail (cadr type-tail) 'java.lang.Object)))
    `(,sname init-form: ,initval type: ,typeval)))

(define (%parse-slot-accessors sdesc)
  (let* ((valvar (gentemp))
         (sname (car sdesc))
         (getter-tail (member getter: sdesc))
         (gettername (if getter-tail (cadr getter-tail) #f))
         (getter (if getter-tail
                     `((,gettername) ,sname)
                     '()))
         (setter-tail (member setter: sdesc))
         (settername (if setter-tail (cadr setter-tail) #f))
         (setter (if setter-tail
                     `((,settername ,valvar) (set! ,sname ,valvar))
                     '())))
    `(,getter ,setter)))

(define (%parse-method-form mdesc)
  mdesc)

(define (%parse-defclass-slots body)
  (let* ((slots-part (assoc slots: body))
         (slot-forms (if slots-part (cdr slots-part) '())))
    (map %parse-slot-description
         slot-forms)))

(define (%parse-defclass-methods body)
  (let* ((slots-part (assoc slots: body))
         (slot-forms (if slots-part (cdr slots-part) '()))
         (slot-accessors (apply append (map %parse-slot-accessors
                                            slot-forms)))
         (methods-part (assoc methods: body))
         (method-specs (if methods-part (cdr methods-part) '()))
         (method-forms (map %parse-method-form method-specs)))
    (append (filter (lambda (sa)(not (null? sa))) slot-accessors)
            (filter (lambda (mf)(not (null? mf))) method-forms))))

(defmacro defclass (classname supers . body)
  (let ((superclasses (%parse-defclass-superclasses supers))
        (slots (%parse-defclass-slots body))
        (methods (%parse-defclass-methods body))
        (init-form (%parse-defclass-init body)))
    `(define-simple-class ,classname ,superclasses
       ,@slots
       ,@methods
       (init: ,init-form))))


