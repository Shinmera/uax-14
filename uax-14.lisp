#|
 This file is a part of UAX-14
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.uax-14
  (:use #:cl)
  (:export
   #:*line-break-database-file*))

(in-package #:org.shirakumo.alloy.uax-14)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults
                                (or *compile-file-truename* *load-truename*
                                    (error "COMPILE-FILE or LOAD this file."))))

(defvar *line-break-database-file* (make-pathname :name "LineBreak" :type "dat" :defaults *here*))

(defmacro defglobal (name value)
  #+sbcl `(sb-ext:defglobal ,name ,value)
  #-sbcl `(defvar ,name ,value))

(defglobal +line-break-type-map+
  #(:XX :BK :CM :CR :GL :LF :NL :SP :WJ :ZW :ZWJ 
    :AI :AL :B2 :BA :BB :CB :CJ :CL :CP :EB :EM
    :EX :H2 :H3 :HL :HY :ID :IN :IS :JL :JT :JV
    :NS :NU :OP :PO :PR :QU :RI :SA :SG :SY))

(declaim (inline type-id))
(defun type-id (type)
  (position type +line-break-type-map+))

(defglobal +line-break-map+ (make-array #x10FFFF :element-type '(unsigned-byte 8)))

(defun load-line-break-database (&optional (source *line-break-database-file*))
  (with-open-file (stream source
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (loop with i = 0
          while (< i (length +line-break-map+))
          do (setf i (read-sequence +line-break-map+ stream :start i)))
    +line-break-map+))

(when (probe-file *line-break-database-file*)
  (load-line-break-database))

(declare (inline char-line-break-id))
(defun char-line-break-id (char)
  (aref +line-break-map+ (char-code char)))

(defun char-line-break-type (char)
  (aref +line-break-type-map+ (char-line-break-id char)))

(defun find-line-breaks (string breaks)
  )
