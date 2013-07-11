;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) json/null->string ()
  ())

(def (projection e) json/boolean->string ()
  ())

(def (projection e) json/number->string ()
  ())

(def (projection e) json/string->string ()
  ())

(def (projection e) json/array->tree/node ()
  ())

(def (projection e) json/object-entry->tree/node ()
  ())

(def (projection e) json/object->tree/node ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/json/null->string ()
  (make-projection 'json/null->string))

(def (function e) make-projection/json/boolean->string ()
  (make-projection 'json/boolean->string))

(def (function e) make-projection/json/number->string ()
  (make-projection 'json/number->string))

(def (function e) make-projection/json/string->string ()
  (make-projection 'json/string->string))

(def (function e) make-projection/json/array->tree/node ()
  (make-projection 'json/array->tree/node))

(def (function e) make-projection/json/object-entry->tree/node ()
  (make-projection 'json/object-entry->tree/node))

(def (function e) make-projection/json/object->tree/node ()
  (make-projection 'json/object->tree/node))

;;;;;;
;;; Construction

(def (macro e) json/null->string ()
  '(make-projection/json/null->string))

(def (macro e) json/boolean->string ()
  '(make-projection/json/boolean->string))

(def (macro e) json/number->string ()
  '(make-projection/json/number->string))

(def (macro e) json/string->string ()
  '(make-projection/json/string->string))

(def (macro e) json/array->tree/node ()
  '(make-projection/json/array->tree/node))

(def (macro e) json/object-entry->tree/node ()
  '(make-projection/json/object-entry->tree/node))

(def (macro e) json/object->tree/node ()
  '(make-projection/json/object->tree/node))

;;;;;;
;;; Printer

;; TODO: rename projections ->tree/leaf

(def printer json/null->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output-content "null")
         (output (make-tree/leaf (make-styled-string/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
         (name-reference `(value (the ,(form-type input) ,input-reference) ,output-content)))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string output-content name-reference 0
                                                   output-content `(content-of (the styled-string/string (content-of (the tree/leaf ,output-reference)))) 0
                                                   (length output-content))))))

(def printer json/boolean->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output-content (boolean-to-string (value-p input)))
         (output (make-tree/leaf (make-styled-string/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/blue*)))
         (name-reference `(boolean-to-string (the boolean (value-p (the ,(form-type input) ,input-reference))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string output-content name-reference 0
                                                   output-content `(content-of (the styled-string/string (content-of (the tree/leaf ,output-reference)))) 0
                                                   (length output-content))))))

(def printer json/number->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output-content (write-to-string (value-of input)))
         (output (make-tree/leaf (make-styled-string/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/magenta*)))
         (value-reference `(write-to-string (the number (value-of (the ,(form-type input) ,input-reference))))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string output-content value-reference 0
                                                   output-content `(content-of (the styled-string/string (content-of (the tree/leaf ,output-reference)))) 0
                                                   (length output-content))))))

(def printer json/string->string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((output-content (text-of input))
         (output (make-tree/leaf (make-styled-string/string output-content :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/green*)
                                 :opening-delimiter (make-styled-string/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :closing-delimiter (make-styled-string/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)))
         (text-reference `(text-of (the ,(form-type input) ,input-reference))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                (make-iomap/string output-content text-reference 0
                                                   output-content `(content-of (the styled-string/string (content-of (the tree/leaf ,output-reference)))) 0
                                                   (length (text-of input)))))))

(def printer json/array->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (deep-array (find-if-not (of-type '(or json/null json/boolean json/number json/string)) (elements-of input)))
         (output (make-tree/node (iter (for element :in-sequence (elements-of input))
                                       (for index :from 0)
                                       (for element-iomap = (recurse-printer recursion iomap element
                                                                             `(elt (the list (elements-of ,typed-input-reference)) ,index)
                                                                             `(elt (the list (children-of (the tree/node ,output-reference))) ,index)))
                                       (push element-iomap child-iomaps)
                                       (when (and deep-array (not (first-iteration-p)))
                                         ;; KLUDGE:
                                         (setf (indentation-of (output-of element-iomap)) 1))
                                       (collect (output-of element-iomap)))
                                 :opening-delimiter (make-styled-string/string "[" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :closing-delimiter (make-styled-string/string "]" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :separator (make-styled-string/string ", " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :indentation (indentation-of input))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

(def printer json/object-entry->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (key (key-of input))
         (iomap (recurse-printer recursion iomap (value-of input)
                                 `(value-of ,typed-input-reference)
                                 `(elt (the list (children-of (the tree/node ,output-reference))) 1)))
         (key-node-reference `(elt (the list (children-of (the tree/node ,output-reference))) 0))
         (output (make-tree/node (list (make-tree/leaf (make-styled-string/string key :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/red*)
                                                       :opening-delimiter (make-styled-string/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                                       :closing-delimiter (make-styled-string/string "\"" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))
                                       (output-of iomap))
                                 :separator (make-styled-string/string " : " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list (make-iomap/object projection recursion input input-reference output output-reference)
                                iomap
                                (make-iomap/object* projection recursion input `(the string (key-of ,typed-input-reference))
                                                    key `(the string (content-of (the tree/leaf ,key-node-reference))))
                                (make-iomap/string key `(key-of ,typed-input-reference) 0
                                                   key `(content-of (the styled-string/string (content-of (the tree/leaf ,key-node-reference)))) 0
                                                   (length key))))))

(def printer json/object->tree/node (projection recursion iomap input input-reference output-reference)
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (output (make-tree/node (iter (for index :from 0)
                                       (for entry :in-sequence (entries-of input))
                                       (for entry-iomap = (recurse-printer recursion iomap entry
                                                                     `(elt (the list (entries-of ,typed-input-reference)) ,index)
                                                                     `(elt (the list (children-of (the tree/node ,output-reference))) ,index)))
                                       (push entry-iomap child-iomaps)
                                       (unless (first-iteration-p)
                                         ;; KLUDGE:
                                         (setf (indentation-of (output-of entry-iomap)) 1))
                                       (collect (output-of entry-iomap)))
                                 :opening-delimiter (make-styled-string/string "{" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :closing-delimiter (make-styled-string/string "}" :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*)
                                 :separator (make-styled-string/string ", " :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/gray*))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader json/null->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader json/boolean->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader json/number->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader json/string->string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader json/array->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader json/object-entry->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)

(def reader json/object->tree/node (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
