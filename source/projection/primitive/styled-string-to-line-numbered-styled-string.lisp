;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Projection

(def (projection e) styled-string->line-numbered-styled-string ()
  ())

;;;;;;
;;; Construction

(def (function e) make-projection/styled-string->line-numbered-styled-string ()
  (make-projection 'styled-string->line-numbered-styled-string))

;;;;;;
;;; Construction

(def (macro e) styled-string->line-numbered-styled-string ()
  '(make-projection/styled-string->line-numbered-styled-string))

;;;;;;
;;; Printer

(def printer styled-string->line-numbered-styled-string (projection recursion iomap input input-reference output-reference)
  (declare (ignore iomap))
  (bind ((typed-input-reference `(the ,(form-type input) ,input-reference))
         (child-iomaps nil)
         (line-count (1+ (styled-string/count input #\NewLine)))
         (line-number-length (1+ (floor (log line-count) (log 10))))
         (element-index 0)
         (string-position 0)
         (elements nil)
         (output (labels ((write-element (element)
                            (push element elements)
                            (incf element-index)
                            (typecase element
                              (styled-string/string
                               (incf string-position (length (content-of element)))))))
                   (iter (with format-string = (format nil "\~~~A,' D " line-number-length))
                         (with input-offset = 0)
                         (with input-index = 0)
                         (for line-index :from 0)
                         (for line :in (styled-string/split input #\NewLine))
                         (for line-number = (format nil format-string (1+ line-index)))
                         (push (make-iomap/string line-number `(line-number ,typed-input-reference ,line-index) 0
                                                  line-number `(content-of (the styled-string/string (elt (the list (elements-of (the styled-string/document ,output-reference))) ,element-index))) 0
                                                  (length line-number))
                               child-iomaps)
                         (write-element (make-styled-string/string line-number :font *font/ubuntu/monospace/regular/18* :font-color *color/solarized/content/light* :fill-color *color/solarized/background/light*))
                         (iter (for line-element :in-sequence (elements-of line))
                               (typecase line-element
                                 (styled-string/string
                                  (for line-content = (content-of line-element))
                                  (push (make-iomap/string line-content `(content-of (the styled-string/string (elt (the list (elements-of ,typed-input-reference)) ,input-index))) 0
                                                           line-content `(content-of (the styled-string/string (elt (the list (elements-of (the styled-string/document ,output-reference))) ,element-index))) 0
                                                           (length line-content))
                                        child-iomaps)))
                               (incf input-index)
                               (write-element line-element))
                         (write-element (make-styled-string/string (string #\NewLine) :font *font/default* :font-color *color/default*))
                         (incf input-offset (1+ (styled-string/length line))))
                   (make-styled-string/document (nreverse elements)))))
    (make-iomap/recursive projection recursion input input-reference output output-reference
                          (list* (make-iomap/object projection recursion input input-reference output output-reference) (nreverse child-iomaps)))))

;;;;;;
;;; Reader

(def reader styled-string->line-numbered-styled-string (projection recursion printer-iomap projection-iomap gesture-queue operation document)
  (declare (ignore projection recursion printer-iomap projection-iomap gesture-queue document))
  operation)
