;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          hgtypes.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       custom types for representing
;;;;                fabric data in the store
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export)

(require "util-java.scm")
(require "util-lists.scm")
(require "util-general.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias ArrayList java.util.ArrayList)
(define-private-alias ByteArrayInputStream java.io.ByteArrayInputStream)
(define-private-alias ByteArrayOutputStream java.io.ByteArrayOutputStream)
(define-private-alias HGAtomType org.hypergraphdb.type.HGAtomType)
(define-private-alias HGPersistentHandle org.hypergraphdb.HGPersistentHandle)
(define-private-alias HyperGraph org.hypergraphdb.HyperGraph)
(define-private-alias IncidenceSetRef org.hypergraphdb.IncidenceSetRef)
(define-private-alias ObjectInputStream java.io.ObjectInputStream)
(define-private-alias ObjectOutputStream java.io.ObjectOutputStream)

;;; ---------------------------------------------------------------------
;;; how to add a custom type to the store
;;; ---------------------------------------------------------------------

;;; add the custom type to the graph store
;;; (define *graph* (HyperGraph "/Users/mikel/Workshop/fabric/db/world.db/"))
;;; (define $typesystem (@ 'getTypeSystem *graph*))
;;; (define $handle-factory (@ 'getHandleFactory *graph*))
;;; (define $custom-type (PersistentSymbolType))
;;; (define $symbol-class Symbol:class)
;;; (define $type-handle (@ 'addPredefinedType $typesystem (@ 'makeHandle $handle-factory) $custom-type $symbol-class))

;;; ---------------------------------------------------------------------
;;; status of types used by the fabric
;;; ---------------------------------------------------------------------
;;; Vector: working
;;; frame: working
;;; UUID: working

;;; ---------------------------------------------------------------------
;;; custom types required by the fabric
;;; ---------------------------------------------------------------------

;;; Symbol
;;; ---------------------------------------------------------------------

(define-simple-class PersistentSymbolType (HGAtomType)
  (graph type: HyperGraph init-form: #!null)

  ((setHyperGraph g::HyperGraph)
   (set! graph g))

  ((store instance) access: 'public
   (let* ((bytes-out::ByteArrayOutputStream (ByteArrayOutputStream))
          (out::ObjectOutputStream (ObjectOutputStream bytes-out)))
     (@ 'writeObject out instance)
     (let ((bytes::byte[] (@ 'toByteArray bytes-out)))
       (@ 'store (@ 'getStore graph) bytes))))
  
  ((release handle::HGPersistentHandle) access: 'public
   (@ 'removeData (@ 'getStore graph) handle))

  ((make handle::HGPersistentHandle targetSet incidenceSet::IncidenceSetRef) access: 'public
   (let* ((bytes::byte[] (@ 'getData (@ 'getStore graph) handle))
          (bytes-in (ByteArrayInputStream bytes))
          (in (ObjectInputStream bytes-in)))
     (@ 'readObject in)))

  ((subsumes general specific) access: 'public
   (@ 'equals general specific)))

;;; Keyword
;;; ---------------------------------------------------------------------

(define-simple-class PersistentKeywordType (HGAtomType)
  (graph type: HyperGraph init-form: #!null)

  ((setHyperGraph g::HyperGraph)
   (set! graph g))

  ((store instance) access: 'public
   (let* ((bytes-out (ByteArrayOutputStream))
          (out (ObjectOutputStream bytes-out)))
     (@ 'writeObject out instance)
     (let ((bytes::byte[] (@ 'toByteArray bytes-out)))
       (@ 'store (@ 'getStore graph) bytes))))
  
  ((release handle::HGPersistentHandle) access: 'public
   (@ 'removeData (@ 'getStore graph) handle))

  ((make handle::HGPersistentHandle targetSet incidenceSet::IncidenceSetRef) access: 'public
   (let* ((bytes::byte[] (@ 'getData (@ 'getStore graph) handle))
          (bytes-in (ByteArrayInputStream bytes))
          (in (ObjectInputStream bytes-in)))
     (@ 'readObject in)))

  ((subsumes general specific) access: 'public
   (@ 'equals general specific)))

;;; List
;;; ---------------------------------------------------------------------
;;; works with the following type mappings:
;;; (define $type-handle (@ 'addPredefinedType $typesystem (@ 'makeHandle $handle-factory) $custom-type gnu.lists.LList:class))
;;; (define $type-handle (@ 'addPredefinedType $typesystem (@ 'makeHandle $handle-factory) $custom-type gnu.lists.Pair:class))
;;; (define $type-handle (@ 'addPredefinedType $typesystem (@ 'makeHandle $handle-factory) $custom-type gnu.lists.PairWithPosition:class))
;;; (define $type-handle (@ 'addPredefinedType $typesystem (@ 'makeHandle $handle-factory) $custom-type gnu.lists.ImmutablePair:class))
;;; (define $type-handle (@ 'addPredefinedType $typesystem (@ 'makeHandle $handle-factory) $custom-type gnu.lists.EmptyList:class))

(define-simple-class PersistentListType (HGAtomType)
  (graph type: HyperGraph init-form: #!null)

  ((setHyperGraph g::HyperGraph)
   (set! graph g))

  ((store instance) access: 'public
   (let* ((storable-instance (list->ArrayList instance))
          (bytes-out (ByteArrayOutputStream))
          (out (ObjectOutputStream bytes-out)))
     (@ 'writeObject out storable-instance)
     (let ((bytes::byte[] (@ 'toByteArray bytes-out)))
       (@ 'store (@ 'getStore graph) bytes))))
  
  ((release handle::HGPersistentHandle) access: 'public
   (@ 'removeData (@ 'getStore graph) handle))

  ((make handle::HGPersistentHandle targetSet incidenceSet::IncidenceSetRef) access: 'public
   (let* ((bytes::byte[] (@ 'getData (@ 'getStore graph) handle))
          (bytes-in (ByteArrayInputStream bytes))
          (in (ObjectInputStream bytes-in))
          (inobj (@ 'readObject in)))
     (ArrayList->list inobj)))

  ((subsumes general specific) access: 'public
   (@ 'equals general specific)))


;;; ---------------------------------------------------------------------
;;; test code
;;; ---------------------------------------------------------------------
;;;
;;; add the custom type to the graph store
;;; (define *graph* (HyperGraph "/Users/mikel/Workshop/fabric/db/world.db/"))
;;; (define $typesystem (@ 'getTypeSystem *graph*))
;;; (define $handle-factory (@ 'getHandleFactory *graph*))
;;; (define $custom-type (PersistentListType))
;;; (define $type-handle (@ 'addPredefinedType $typesystem (@ 'makeHandle $handle-factory) $custom-type gnu.lists.LList:class))
;;; (define $type-handle (@ 'addPredefinedType $typesystem (@ 'makeHandle $handle-factory) $custom-type gnu.lists.Pair:class))
;;; (define $type-handle (@ 'addPredefinedType $typesystem (@ 'makeHandle $handle-factory) $custom-type gnu.lists.PairWithPosition:class))
;;; (define $type-handle (@ 'addPredefinedType $typesystem (@ 'makeHandle $handle-factory) $custom-type gnu.lists.ImmutablePair:class))
;;; (define $type-handle (@ 'addPredefinedType $typesystem (@ 'makeHandle $handle-factory) $custom-type gnu.lists.EmptyList:class))
;;; (@ 'close *graph*)
;;;
;;; add an object to the graph
;;; (define *graph* (HyperGraph "/Users/mikel/Workshop/fabric/db/world.db/"))
;;; (define $listoid (@ 'add *graph* (list 1 2 3)))
;;; (@ 'get *graph* $listoid)
;;;
;;; close the graph, open it, and find the object
;;; (@ 'close *graph*)
;;; (set! *graph* (HyperGraph "/Users/mikel/Workshop/fabric/db/world.db/"))
;;; (define $found (HGQ:getOne *graph* (HGQ:type gnu.lists.Pair:class)))
;;; (@ 'close *graph*)
