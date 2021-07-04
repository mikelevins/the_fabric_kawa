;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          fabric.el
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       emacs tools setup
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; customizations for managing the Fabric development environment
;;; run the fabric engine and connect to it

;;; ---------------------------------------------------------------------
;;; setup site-specific variables
;;; ---------------------------------------------------------------------

;;; (load "/Users/mikel/Workshop/src/the_fabric/emacs/fabric.el")

;;; find this file
(defvar $this-file-path nil)
(setq $this-file-path (or (symbol-file '$this-file-path)
			  load-file-name))

;;; init the fabric's emacs path
(defvar $fabric-emacs-path (file-name-directory $this-file-path))

;;; deduce other project paths
(defvar $fabric-root (expand-file-name (concat $fabric-emacs-path "../")))
(defvar $fabric-bin (expand-file-name (concat $fabric-root "bin/")))
(defvar $fabric-kawa-script (expand-file-name (concat $fabric-bin "kawa")))

;;; adjust emacs
(add-to-list 'load-path (expand-file-name $fabric-emacs-path))

;;; ---------------------------------------------------------------------
;;; Customizing Quack 
;;; ---------------------------------------------------------------------
;;; to change these settings, copy the expressions to your .emacs,
;;; uncomment them, and change the values to what you want

;;; Option menu settings and programs persist using the `custom' facility.
;;; (set 'quack-options-persist-p t)

;;; Whether to have a \"Quack\" menu always on the menu bar.
;;; (set 'quack-global-menu-p t)

;;; Whether Quack should avoid use of Tab characters in indentation.
;;; (set 'quack-tabs-are-evil-p t)

;;; Whether to have a \"Quack\" menu always on the menu bar
;;; (set 'quack-global-menu-p t)

;;; Which font-lock fontification style to use. Options are plt,
;;; emacs, and nil
;;; (set 'quack-fontify-style 'plt)

;;; If non-nil, fontify names in definition forms for PLT-style
;;; fontification.  This only has effect when quack-fontify-style is plt
;;;(set 'quack-pltish-fontify-definition-names-p t)

;;; Whether three-semicolon comments should be fontified differently.
;;; (set 'quack-fontify-threesemi-p t)



;;; ---------------------------------------------------------------------
;;; run the fabric's Scheme 
;;; ---------------------------------------------------------------------

;;; run the fabric Scheme environment in quack mode
(defun run-the-fabric ()
  (interactive)
  ;;(require 'quack)
  (require 'cmuscheme)
  (let ((scheme-program-name $fabric-kawa-script))
    (setenv "FABRIC_ROOT" $fabric-root)
    (run-scheme scheme-program-name)))





