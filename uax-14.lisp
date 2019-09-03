#|
 This file is a part of UAX-14
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.uax-14
  (:use #:cl)
  (:export
   #:*line-break-database-file*
   #:*pair-table-file*))

(in-package #:org.shirakumo.alloy.uax-14)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults
                                (or *compile-file-truename* *load-truename*
                                    (error "COMPILE-FILE or LOAD this file."))))

(defvar *line-break-database-file* (make-pathname :name "LineBreak" :type "dat" :defaults *here*))
(defvar *pair-table-file* (make-pathname :name "PairTable" :type "dat" :defaults *here*))

(defmacro defglobal (name value)
  #+sbcl `(sb-ext:defglobal ,name ,value)
  #-sbcl `(defvar ,name ,value))

(defglobal +line-break-type-map+
    #(:OP :CL :CP :QU :GL :NS :EX :SY :IS :PR :PO :NU :AL :HL :ID
      :IN :HY :BA :BB :B2 :ZW :CM :WJ :H2 :H3 :JL :JV :JT :RI :EB
      :EM :ZWJ :CB :AI :BK :CJ :CR :LF :NL :SA :SG :SP :XX))

(declaim (inline type-id))
(defun type-id (type)
  (position type +line-break-type-map+))

(defglobal +pair-type-map+
    #(:DI :IN :CI :CP :PR))

(declaim (inline pair-id))
(defun pair-id (pair)
  (position pair +pair-type-map+))

(defglobal +line-break-map+ (make-array #x10FFFF :element-type '(unsigned-byte 8) :initial-element 0))

(defun load-line-break-database (&optional (source *line-break-database-file*))
  (with-open-file (stream source
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (loop with i = 0
          while (< i (length +line-break-map+))
          do (setf i (read-sequence +line-break-map+ stream :start i)))
    +line-break-map+))

(declaim (inline char-line-break-id))
(defun char-line-break-id (char)
  (aref +line-break-map+ (char-code char)))

(defun char-line-break-type (char)
  (aref +line-break-type-map+ (char-line-break-id char)))

(defglobal +pair-table+ (make-array (expt (length +line-break-type-map+) 2) :element-type '(unsigned-byte 8) :initial-element 0))

(defun load-pair-table (&optional (source *pair-table-file*))
  (with-open-file (stream source
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (loop with i = 0
          while (< i (length +pair-table+))
          do (setf i (read-sequence +pair-table+ stream :start i)))
    +pair-table+))

(declaim (inline pair-type-id))
(defun pair-type-id (b a)
  (aref +pair-table+ (+ (char-line-break-id a)
                        (* (char-line-break-id b)
                           (length +line-break-type-map+)))))

(defun pair-type (b a)
  (aref +pair-type-map+ (pair-type-id b a)))

(defun find-line-breaks (string breaks)
  )



;;; Load the tables.
(cond ((and (probe-file *line-break-database-file*)
            (probe-file *pair-table-file*))
       (load-line-break-database)
       (load-pair-table))
      (T
       (format T "~&UAX-14: Database files are not available. Please run the UAX-14 compiler.")))
