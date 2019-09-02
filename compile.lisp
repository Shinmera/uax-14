#|
 This file is a part of UAX-14
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.uax-14.compiler
  (:use #:cl)
  (:local-nicknames
   (#:uax-14 #:org.shirakumo.alloy.uax-14))
  (:export))

(in-package #:org.shirakumo.alloy.uax-14.compiler)

(defvar *defaults*
  '((#x20A0 #x20CF :PR)
    (#x3400 #x4DBF :ID)
    (#x4E00 #x9FFF :ID)
    (#xF900 #xFAFF :ID)
    (#x1F000 #x1FFFD :ID)
    (#x20000 #x2FFFD :ID)
    (#x30000 #x3FFFD :ID)))

(defun set-default-types (data)
  (loop for (start end type) in *defaults*
        for value = (uax-14::type-id type)
        do (loop for i from start to end
                 do (setf (aref data i) value))))

(defun set-line-types (data line)
  (let* ((end (or (position #\Space line) (position #\# line) (length line)))
         (sep (position #\; line))
         (dot (position #\. line))
         (type (string-upcase (subseq line (1+ sep) end)))
         (value (uax-14::type-id (find-symbol type "KEYWORD")))
         (start (parse-integer line :radix 16 :start 0 :end (or dot sep)))
         (end (if dot (parse-integer line :radix 16 :start (+ 2 dot) :end sep) start)))
    (loop for i from start to end
          do (setf (aref data i) value))))

(defun compile-line-break-database (&key (source (make-pathname :name "LineBreak" :type "txt" :defaults uax-14::*here*))
                                         (target uax-14:*line-break-database-file*))
  (let ((data (make-array #x10FFFF :element-type '(unsigned-byte 8)
                                   :initial-element (uax-14::type-id :XX))))
    (set-default-types data)
    (with-open-file (stream source
                            :direction :input
                            :element-type 'character
                            :external-format :utf-8)
      (loop for line = (read-line stream NIL)
            while line
            do (when (and (string/= "" line)
                          (char/= #\# (char line 0)))
                 (set-line-types data line))))
    (with-open-file (stream target
                            :direction :output
                            :element-type '(unsigned-byte 8))
      (write-sequence data stream))
    target))

(defun split (split string)
  (let ((parts ()) (buffer (make-string-output-stream)))
    (flet ((maybe-output ()
             (let ((part (get-output-stream-string buffer)))
               (when (string/= part "") (push part parts)))))
      (loop for char across string
            do (if (char= char split)
                   (maybe-output)
                   (write-char char buffer))
            finally (maybe-output))
      (nreverse parts))))

(defun compile-pair-table (&key (source (make-pathname :name "PairTable" :type "txt" :defaults uax-14::*here*))
                                (target uax-14:*pair-table-file*))
  (let ((data (make-array (expt (length uax-14::+line-break-type-map+) 2)
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (with-open-file (stream source
                            :direction :input
                            :element-type 'character
                            :external-format :utf-8)
      (let ((As (cdr (split #\Tab (read-line stream)))))
        (loop for line = (read-line stream NIL)
              while line
              do (destructuring-bind (B . Vs) (split #\Tab line)
                   (let ((row (uax-14::type-id (find-symbol B "KEYWORD"))))
                     (loop for A in As
                           for V in Vs
                           for col = (uax-14::type-id (find-symbol A "KEYWORD"))
                           for idx = (+ col (* row (length uax-14::+line-break-type-map+)))
                           do (setf (aref data idx) (uax-14::pair-id (character V)))))))
        (with-open-file (stream target
                                :direction :output
                                :element-type '(unsigned-byte 8))
          (write-sequence data stream))
        target))))
