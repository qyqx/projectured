;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) book/book->tree/node ()
  ())

(def (projection e) book/chapter->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/book/book->tree/node ()
  (make-projection 'book/book->tree/node))

(def (function e) make-projection/book/chapter->tree/node ()
  (make-projection 'book/chapter->tree/node))

;;;;;;
;;; Printer

(def printer book/book->tree/node (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((child-iomaps nil)
         (output (bind ((typed-input-reference `(the ,(form-type input) ,input-reference)))
                   (make-tree/node (list* (bind ((title (title-of input))
                                                 (output (make-tree/leaf title)))
                                            (push (make-iomap/object projection recursion
                                                                     title `(title-of ,typed-input-reference)
                                                                     output `(elt (the list (children-of (the tree/node ,output-reference))) 0))
                                                  child-iomaps)
                                            (push (make-iomap/string title `(title-of ,typed-input-reference) 0
                                                                     title `(content-of (the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) 0))) 0
                                                                     (length title))
                                                  child-iomaps)
                                            output)
                                          (iter (for index :from 0)
                                                (for element :in-sequence (elements-of input))
                                                (for iomap = (recurse-printer recursion iomap element
                                                                              `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                              `(elt (the list (children-of (the tree/node ,output-reference))) ,(1+ index))))
                                                (push iomap child-iomaps)
                                                (collect (output-of iomap))))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 (reverse child-iomaps)))))

(def printer book/chapter->tree/node (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((child-iomaps nil)
         (output (bind ((typed-input-reference `(the ,(form-type input) ,input-reference)))
                   (make-tree/node (list* (bind ((title (title-of input))
                                                 (output (make-tree/leaf title)))
                                            (push (make-iomap/object projection recursion
                                                                     title `(title-of ,typed-input-reference)
                                                                     output `(elt (the list (children-of (the tree/node ,output-reference))) 0))
                                                  child-iomaps)
                                            (push (make-iomap/string title `(title-of ,typed-input-reference) 0
                                                                     title `(content-of (the tree/leaf (elt (the list (children-of (the tree/node ,output-reference))) 0))) 0
                                                                     (length title))
                                                  child-iomaps)
                                            output)
                                          (iter (for index :from 0)
                                                (for element :in-sequence (elements-of input))
                                                (for iomap = (recurse-printer recursion iomap element
                                                                              `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                              `(elt (the list (children-of (the tree/node ,output-reference))) ,(1+ index))))
                                                (push iomap child-iomaps)
                                                (collect (output-of iomap))))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference)
                                 (reverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader book/book->tree/node (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)

(def reader book/chapter->tree/node (projection recursion input input-reference output-reference)
  (declare (ignore projection recursion input input-reference output-reference))
  nil)
