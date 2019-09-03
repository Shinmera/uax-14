#|
 This file is a part of UAX-14
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.uax-14)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults
                                (or *compile-file-truename* *load-truename*
                                    (error "COMPILE-FILE or LOAD this file."))))

(defvar *line-break-database-file* (make-pathname :name "LineBreak" :type "dat" :defaults *here*))
(defvar *pair-table-file* (make-pathname :name "PairTable" :type "dat" :defaults *here*))

(deftype idx ()
  '(integer 0 #.ARRAY-DIMENSION-LIMIT))

(deftype code ()
  '(integer 0 #x10FFFF))

(defmacro defglobal (name value)
  #+sbcl `(sb-ext:defglobal ,name ,value)
  #-sbcl `(defvar ,name ,value))

(declaim (type (simple-array keyword) +line-break-type-map+))
(defglobal +line-break-type-map+
    #(:OP :CL :CP :QU :GL :NS :EX :SY :IS :PR :PO :NU :AL :HL :ID
      :IN :HY :BA :BB :B2 :ZW :CM :WJ :H2 :H3 :JL :JV :JT :RI :EB
      :EM :ZWJ :CB :AI :BK :CJ :CR :LF :NL :SA :SG :SP :XX))

(declaim (inline type-id))
(defun type-id (type)
  (position type +line-break-type-map+))

(define-compiler-macro type-id (&whole whole type &environment env)
  (if (constantp type env)
      `(load-time-value (position ,type +line-break-type-map+))
      whole))

(declaim (type (simple-array keyword) +pair-type-map+))
(defglobal +pair-type-map+
    #(:DI :IN :CI :CP :PR))

(declaim (inline pair-id))
(defun pair-id (pair)
  (position pair +pair-type-map+))

(define-compiler-macro pair-id (&whole whole pair &environment env)
  (if (constantp pair env)
      `(load-time-value (position ,pair +pair-type-map+))
      whole))

(declaim (type (simple-array (unsigned-byte 8) (#x10FFFF)) +line-break-map+))
(defglobal +line-break-map+ (make-array #x10FFFF :element-type '(unsigned-byte 8) :initial-element 0))

(defun load-line-break-database (&optional (source *line-break-database-file*))
  (with-open-file (stream source
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (loop with i = 0
          while (< i (length +line-break-map+))
          do (setf i (read-sequence +line-break-map+ stream :start i)))
    +line-break-map+))

(declaim (ftype (function (code) (unsigned-byte 8)) line-break-id))
(declaim (inline line-break-id))
(defun line-break-id (id)
  (aref +line-break-map+ id))

(defun char-line-break-type (char)
  (aref +line-break-type-map+ (line-break-id (char-code char))))

(declaim (type (simple-array (unsigned-byte 8)) +pair-table+))
(defglobal +pair-table+ (make-array (expt (length +line-break-type-map+) 2) :element-type '(unsigned-byte 8) :initial-element 0))

(defun load-pair-table (&optional (source *pair-table-file*))
  (with-open-file (stream source
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (loop with i = 0
          while (< i (length +pair-table+))
          do (setf i (read-sequence +pair-table+ stream :start i)))
    +pair-table+))

(declaim (ftype (function ((unsigned-byte 8) (unsigned-byte 8)) (unsigned-byte 8)) pair-type-id))
(declaim (inline pair-type-id))
(defun pair-type-id (b a)
  (aref +pair-table+ (+ a (* b (the (unsigned-byte 8) (length +line-break-type-map+))))))

(defun pair-type (b a)
  (aref +pair-type-map+ (pair-type-id (char-line-break-id b) (char-line-break-id a))))

(defun load-databases ()
  (restart-case
      (cond ((and (probe-file *line-break-database-file*)
                  (probe-file *pair-table-file*))
             (load-line-break-database)
             (load-pair-table)
             T)
            (T
             (error "Database files not available. ~%Please run ~s" 'compile-databases)))
    (compile ()
      :report "Compile the databases and retry."
      (compile-databases))
    (abort ()
      :report "Ignore the missing files. UAX-14 will not work correctly."
      NIL)))

(defun compile-databases ()
  (load (make-pathname :name "compile" :type "lisp" :defaults *here*))
  (funcall (find-symbol (string 'compile-databases) '#:org.shirakumo.alloy.uax-14.compiler))
  (load-databases))

(handler-case (load-databases)
  (error (e)
    (format T "~&UAX-14: ~a" e)))
