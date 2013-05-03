;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document tree/base ()
  ((opening-delimiter :type string)
   (closing-delimiter :type string)
   (indentation :type positive-integer)))

(def document tree/leaf (tree/base)
  ((content :type t)))

(def document tree/node (tree/base)
  ((expanded :type boolean)
   (separator :type string)
   (children :type sequence)))

;;;;;;
;;; Construction

(def (function e) make-tree/leaf (content &key opening-delimiter closing-delimiter indentation)
  (make-instance 'tree/leaf
                 :content content
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :indentation indentation))

(def (function e) make-tree/node (children &key opening-delimiter closing-delimiter separator indentation)
  (make-instance 'tree/node
                 :children children
                 :opening-delimiter opening-delimiter
                 :closing-delimiter closing-delimiter
                 :indentation indentation
                 :expanded #t
                 :separator separator))

;;;;;;
;;; Construction

(def (macro e) tree/leaf (() content)
  `(make-tree/leaf ,content))

(def (macro e) tree/node (() &body children)
  `(make-tree/node (list ,@children)))

;;;;;;
;;; Reference

(def macro opening-delimiter (reference value)
  (declare (ignore reference))
  value)

(def macro closing-delimiter (reference value)
  (declare (ignore reference))
  value)

(def macro separator (previous-child-reference next-child-reference value)
  (declare (ignore previous-child-reference next-child-reference))
  value)

(def macro indentation (reference value)
  (declare (ignore reference))
  value)

(def (function e) new-line (reference)
  (declare (ignore reference))
  "
")

;;;;;;
;;; Operation

(def operation operation/tree (operation)
  ())

(def operation operation/tree/toggle-node (operation/tree)
  ((document :type document)
   (target :type reference)))

;;;;;;
;;; Construction

(def (function e) make-operation/tree/toggle-node (document target)
  (make-instance 'operation/tree/toggle-node :document document :target target))

;;;;;;
;;; Redo

(def method redo-operation ((operation operation/tree/toggle-node))
  (notf (expanded-p (eval-reference (document-of operation) (target-of operation)))))

;;;;;;
;;; Provider

(def (function e) tree-font-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the character (elt (the string (content-of (the tree/leaf ?a))) 0))
                     (return-from tree-font-provider *font/ubuntu/monospace/bold/18*))))))

(def (function e) tree-font-color-provider (iomap reference)
  (map-backward iomap reference
                (lambda (iomap reference)
                  (declare (ignore iomap))
                  (pattern-case reference
                    ((the character (elt (the string (?or (opening-delimiter ?a ?b)
                                                          (closing-delimiter ?a ?b))) ?c))
                     (return-from tree-font-color-provider (make-style/color 255 196 196 196)))))))

(def (function e) tree-delimiter-provider (iomap reference)
  (declare (ignore iomap))
  (pattern-case reference
    ((?or (opening-delimiter ?node)
          (closing-delimiter ?node))
     (bind ((delimiter (first reference)))
       (pattern-case ?node
         ((the tree/node ?a)
          (return-from tree-delimiter-provider
            (ecase delimiter
              (opening-delimiter "(")
              (closing-delimiter ")")))))))))

(def (function e) tree-separator-provider (iomap previous-child-reference next-child-reference)
  (declare (ignore iomap previous-child-reference))
  (pattern-case next-child-reference
    ((the ?a (elt (the list (children-of (the tree/node ?b))) ?c))
     (return-from tree-separator-provider " "))))

(def (function e) tree-indentation-provider (iomap previous-child-reference next-child-reference parent-node)
  (declare (ignore iomap previous-child-reference))
  (when (some (of-type 'tree/node) (children-of parent-node))
    (pattern-case next-child-reference
      ((the ?a (elt (the list (children-of (the tree/node ?b))) ?c))
       (when (> ?c 0)
         (return-from tree-indentation-provider 2))))))
