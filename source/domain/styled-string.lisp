;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Data structure

(def document styled-string/base ()
  ())

(def document styled-string/document (styled-string/base)
  ((elements :type sequence)))

(def document styled-string/string (styled-string/base)
  ((content :type string)
   (font :type style/font)
   (font-color :type style/color)
   (fill-color :type style/color)
   (line-color :type style/color)))

;;;;;;
;;; Construction

(def (function e) make-styled-string/document (elements)
  (make-instance 'styled-string/document :elements elements))

(def (function e) make-styled-string/string (content &key font font-color fill-color line-color)
  (make-instance 'styled-string/string
                 :content content
                 :font font
                 :font-color font-color
                 :fill-color fill-color
                 :line-color line-color))

;;;;;;
;;; Construction

(def (macro e) styled-string/document (&body elements)
  `(make-styled-string/document (list ,@elements)))

(def (macro e) styled-string/string (content &key font font-color fill-color line-color)
  `(make-styled-string/string ,content :font ,font :font-color ,font-color :fill-color ,fill-color :line-color ,line-color))

(def (macro e) styled-string/newline ()
  `(make-styled-string/string "
" :font *font/default* :font-color *color/default*))

;;;;;;
;;; API

(def (function e) styled-string/length (styled-string)
  (iter (for element :in-sequence (elements-of styled-string))
        (summing
         (typecase element
           (styled-string/string
            (length (content-of element)))
           (t 0)))))

(def (function e) styled-string/substring (styled-string start-element-index start-character-index end-element-index end-character-index)
  (make-styled-string/document
   (iter (with elements = (elements-of styled-string))
         (with elements-length = (length elements))
         (for element-index :from start-element-index :to end-element-index)
         (until (= element-index elements-length))
         (for element = (elt elements element-index))
         (typecase element
           (styled-string/string
            (bind ((content (content-of element))
                   (content-length (length content))
                   (element-start-character-index (if (= element-index start-element-index)
                                                      start-character-index
                                                      0))
                   (element-end-character-index (if (= element-index end-element-index)
                                                    end-character-index
                                                    content-length)))
              (if (and (= element-start-character-index 0)
                       (= element-end-character-index content-length))
                  (collect element)
                  (bind ((word-part (subseq content element-start-character-index element-end-character-index)))
                    (unless (zerop (length word-part))
                      (collect (make-styled-string/string word-part
                                                          :font (font-of element)
                                                          :font-color (font-color-of element)
                                                          :fill-color (fill-color-of element)
                                                          :line-color (line-color-of element))))))))
           (t
            (collect element))))))

(def (function e) styled-string/find (styled-string start-element-index start-character-index test)
  (iter (with elements = (elements-of styled-string))
        (with element-index = start-element-index)
        (for element = (elt elements element-index))
        (for character-index :from start-character-index)
        (typecase element
          (styled-string/string
           (for content = (content-of element))
           (when (= character-index (length content))
             (setf character-index -1)
             (incf element-index)
             (if (= element-index (length elements))
                 (return (values element-index 0))
                 (next-iteration)))
           (when (funcall test (elt content character-index))
             (return (values element-index character-index))))
          (t
           (setf character-index -1)
           (incf element-index)))))

(def (function e) styled-string/count (styled-string character)
  (iter (for element :in-sequence (elements-of styled-string))
        (summing
         (typecase element
           (styled-string/string
            (funcall 'count character (content-of element)))
           (t 0)))))

(def (function e) styled-string/as-string (styled-string)
  (with-output-to-string (stream)
    (iter (for element :in-sequence (elements-of styled-string))
          (typecase element
            (styled-string/string
             (write-string (content-of element) stream))))))

(def (function e) styled-string/split (styled-string split-character)
  (iter (with length = (styled-string/length styled-string))
        (with start-element-index = 0)
        (with start-character-index = 0)
        (for (values end-element-index end-character-index) = (styled-string/find styled-string start-element-index start-character-index (lambda (character) (char= character split-character))))
        (collect (styled-string/substring styled-string start-element-index start-character-index end-element-index end-character-index))
        (for index = (1+ (styled-string/index styled-string end-element-index end-character-index)))
        (while (< index length))
        (setf start-element-index (styled-string/element-index styled-string index))
        (setf start-character-index (styled-string/character-index styled-string index))))

(def (function e) styled-string/concatenate (&rest style-strings)
  (make-styled-string/document (apply #'append (mapcar #'elements-of style-strings))))

(def (function e) styled-string/element-index (styled-string index)
  (iter (for element-index :from 0)
        (for element :in-sequence (elements-of styled-string))
        (typecase element
          (styled-string/string (decf index (length (content-of element)))))
        (when (<= index 0)
          (return element-index))))

(def (function e) styled-string/character-index (styled-string index)
  (iter (for element :in-sequence (elements-of styled-string))
        (typecase element
          (styled-string/string
           (for length = (length (content-of element)))
           (if (<= index length)
               (return index)
               (decf index length))))))

(def (function e) styled-string/index (styled-string element-index character-index)
  (+ (iter (for index :from 0 :below element-index)
           (for element :in-sequence (elements-of styled-string))
           (summing
            (typecase element
              (styled-string/string
               (length (content-of element)))
              (t 0))))
     character-index))
