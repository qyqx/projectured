;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) text/document->tree/node ()
  ())

(def (projection e) text/paragraph->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/text/document->tree/node ()
  (make-projection 'text/document->tree/node))

(def (function e) make-projection/text/paragraph->tree/node ()
  (make-projection 'text/paragraph->tree/node))

;;;;;;
;;; Printer

(def printer text/document->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in-sequence (elements-of input))
                                       (for element-iomap = (recurse-printer recursion iomap element
                                                                             `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                             `(elt (the list (children-of (the tree/node ,output-reference))) ,(1+ index))))
                                       (push element-iomap child-iomaps)
                                       ;; KLUDGE:
                                       (setf (indentation-of (output-of element-iomap)) 2)
                                       (collect (output-of element-iomap))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 (reverse child-iomaps)))))

(def printer text/paragraph->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((child-iomaps nil)
         (typed-input-reference `(the ,(form-type input) ,input-reference))
         (output (make-tree/node (iter (for index :from 0)
                                       (for element :in-sequence (elements-of input))
                                       (for element-iomap = (recurse-printer recursion iomap element
                                                                             `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                             `(elt (the list (children-of (the tree/node ,output-reference))) ,(1+ index))))
                                       (push element-iomap child-iomaps)
                                       ;; KLUDGE:
                                       (setf (indentation-of (output-of element-iomap)) 2)
                                       (collect (output-of element-iomap))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 (reverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader text/document->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader text/paragraph->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
