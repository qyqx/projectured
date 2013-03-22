;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured.test)

(def suite* (test :in root-suite))

;; TODO: split and move to where iomaps are defined
(def function map-input-references (iomap function)
  (etypecase iomap
    (iomap/object
     (when-bind input-reference (input-reference-of iomap)
       (funcall function iomap input-reference)))
    (iomap/string
     (bind ((length (length-of iomap)))
       (iter (for index :from 0 :below length)
             (funcall function iomap `(the character (elt ,(input-reference-of iomap) ,(+ index (input-offset-of iomap))))))
       #+nil
       (iter (for index :from 0 :to length)
             (funcall function iomap `(the sequence-position (pos ,(input-reference-of iomap) ,(+ index (input-offset-of iomap))))))))
    (iomap/sequential
     (map-input-references (first (element-iomaps-of iomap)) function))
    (iomap/recursive
     (iter (for child-iomap :in (child-iomaps-of iomap))
           (map-input-references child-iomap function)))))

;; TODO: split and move to where iomaps are defined
(def function map-output-references (iomap function)
  (etypecase iomap
    (iomap/object
     (when-bind output-reference (output-reference-of iomap)
       (funcall function iomap output-reference)))
    (iomap/string
     (bind ((length (length-of iomap)))
       (iter (for index :from 0 :below length)
             (funcall function iomap `(the character (elt ,(output-reference-of iomap) ,(+ index (output-offset-of iomap))))))
       #+nil
       (iter (for index :from 0 :to length)
             (funcall function iomap `(the sequence-position (pos ,(output-reference-of iomap) ,(+ index (output-offset-of iomap))))))))
    (iomap/sequential
     (map-output-references (last-elt (element-iomaps-of iomap)) function))
    (iomap/recursive
     (iter (for child-iomap :in (child-iomaps-of iomap))
           (map-output-references child-iomap function)))))

(def function print-input-references (iomap)
  (map-input-references iomap (lambda (iomap reference)
                                (declare (ignore iomap))
                                (print reference))))

(def function print-output-references (iomap)
  (map-output-references iomap (lambda (iomap reference)
                                 (declare (ignore iomap))
                                 (print reference))))

(def function print-references-backward (iomap &key (reference #t) evaluate apply)
  (map-output-references iomap (lambda (reference-iomap output-reference)
                                 (declare (ignore reference-iomap))
                                 (when reference
                                   (format t "~%~%Reference: ~S" output-reference))
                                 (when evaluate
                                   (format t "~%Evaluate: ~S" (eval-reference (input-of iomap) output-reference)))
                                 (when apply
                                   (format t "~%Apply: ~S" (apply-reference iomap output-reference nil)))
                                 (map-backward iomap output-reference
                                               (lambda (reference-iomap input-reference)
                                                 (declare (ignore reference-iomap))
                                                 (when reference
                                                   (format t "~%Reference: ~S" input-reference))
                                                 (when evaluate
                                                   (format t "~%Evaluate: ~S" (eval-reference (input-of iomap) input-reference)))
                                                 (when apply
                                                   (format t "~%Apply: ~S" (apply-reference iomap input-reference nil))))))))

(def function print-references-forward (iomap &key (reference #t) evaluate apply)
  (map-input-references iomap (lambda (reference-iomap input-reference)
                                (declare (ignore reference-iomap))
                                (when reference
                                  (format t "~%~%Reference: ~S" input-reference))
                                (when evaluate
                                  (format t "~%Evaluate: ~S" (eval-reference (input-of iomap) input-reference)))
                                (when apply
                                  (format t "~%Apply: ~S" (apply-reference iomap input-reference nil)))
                                (map-forward iomap input-reference
                                             (lambda (reference-iomap output-reference)
                                               (declare (ignore reference-iomap))
                                               (when reference
                                                 (format t "~%Reference: ~S" output-reference))
                                               (when evaluate
                                                 (format t "~%Evaluate: ~S" (eval-reference (input-of iomap) output-reference)))
                                               (when apply
                                                 (format t "~%Apply: ~S" (apply-reference iomap output-reference nil))))))))
