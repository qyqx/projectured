;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document json/base ()
  ())

(def document json/null (json/base)
  ())

(def document json/boolean (json/base)
  ((value :type boolean)))

(def document json/number (json/base)
  ((value :type number)))

(def document json/string (json/base)
  ((text :type string)))

(def document json/array (json/base)
  ((elements :type sequence)))

(def document json/object-entry (json/base)
  ((key :type t)
   (value :type t)))

(def document json/object (json/base)
  ((entries :type sequence)))

;;;;;;
;;; Construction

(def (function e) make-json/null ()
  (make-instance 'json/null))

(def (function e) make-json/boolean (value)
  (make-instance 'json/boolean :value value))

(def (function e) make-json/number (value)
  (make-instance 'json/number :value value))

(def (function e) make-json/string (text)
  (make-instance 'json/string :text text))

(def (function e) make-json/array (elements)
  (make-instance 'json/array :elements elements))

(def (function e) make-json/object-entry (key value)
  (make-instance 'json/object-entry :key key :value value))

(def (function e) make-json/object (entries)
  (make-instance 'json/object :entries entries))

;;;;;;
;;; Construction

(def (macro e) json/null ()
  '(make-json/null))

(def (macro e) json/boolean (value)
  `(make-json/boolean ,value))

(def (macro e) json/number (value)
  `(make-json/number ,value))

(def (macro e) json/string (text)
  `(make-json/string ,text))

(def (macro e) json/array (&body elements)
  `(make-json/array (list ,@elements)))

(def (macro e) json/object-entry (key value)
  `(make-json/object-entry ,key ,value))

(def (macro e) json/object (&body key-value-pairs)
  `(make-json/object (list ,@(iter (for (key value) :in key-value-pairs)
                                   (collect `(make-json/object-entry ,key ,value))))))
