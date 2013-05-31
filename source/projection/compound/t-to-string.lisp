;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

(def method make-projection/t->string ((instance string))
  (preserving))

(def method make-projection/t->string ((instance text/base))
  (recursive (text->string)))

(def method make-projection/t->string ((instance list/base))
  (recursive (list->string)))

(def method make-projection/t->string ((instance table/table))
  (recursive (table->string)))

(def method make-projection/t->string ((instance tree/node))
  (tree->string))

(def method make-projection/t->string ((instance book/base))
  (sequential
    (recursive (book->tree))
    (recursive (tree->styled-string))
    (styled-string->string)))

(def method make-projection/t->string ((instance xml/base))
  (sequential
    (recursive (xml->tree))
    (recursive (tree->styled-string))
    (styled-string->string)))

(def method make-projection/t->string ((instance java/base))
  (sequential
    (recursive (java->tree))
    (recursive (tree->styled-string))
    (styled-string->string)))

(def method make-projection/t->string ((instance json/base))
  (sequential
    (recursive (json->tree))
    (recursive (tree->styled-string))
    (styled-string->string)))

(def method make-projection/t->string ((instance sequence))
  (sequential
    (recursive (lisp-form->tree))
    (tree->string)))

(def method make-projection/t->string ((instance hu.dwim.walker::walked-form))
  (sequential
    (recursive (walked-lisp-form->lisp-form))
    (recursive (lisp-form->tree))
    (tree->string)))
