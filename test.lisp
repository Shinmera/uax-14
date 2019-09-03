#|
 This file is a part of UAX-14
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.uax-14.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:uax-14 #:org.shirakumo.alloy.uax-14))
  (:export #:uax-14))

(in-package #:org.shirakumo.alloy.uax-14.test)

(defvar *test-file* (make-pathname :name "LineBreakTest" :type "txt" :defaults uax-14::*here*))
(defvar *skipped* #(1140 1142 1144 1146 1308 1310 1312 1314 2980 2982 4496 4498 4664 4666 5164 5166
                    7136 7145 7150 7235 7236 7237 7238 7239 7240 7242 7243 7244 7245 7246))

(defun parse-code (code)
  (parse-integer code :radix 16))

(define-test uax-14)

(defun string-breaks (string)
  (loop with breaker = (uax-14:make-breaker string)
        for last = 0 then pos
        for (pos weak) = (multiple-value-list (uax-14:next-break breaker))
        while pos
        collect (subseq string last pos)))

(define-test unicode-suite
  :parent uax-14
  (with-open-file (stream *test-file*
                          :direction :input
                          :element-type 'character
                          :external-format :utf-8)
    (loop for i from 0 below 100
          for line = (read-line stream NIL)
          while line
          do (let ((comment (position #\# line)))
               (when (and (string/= "" line)
                          (or (null comment) (< 0 comment)))
                 (let* ((expected (loop for part in (cl-ppcre:split "\\s*÷\\s*" line :end (or comment (length line)))
                                        for points = (cl-ppcre:split "\\s*×\\s*" part)
                                        do (when (string= "" (first points))
                                             (pop points))
                                        collect (let ((codes (mapcar #'parse-code points)))
                                                  (map 'string #'code-char codes))))
                        (points (cl-ppcre:split "\\s*[×÷]\\s*" line :end (or comment (length line))))
                        (codes (mapcar #'parse-code (rest points)))
                        (string (map 'string #'code-char codes))
                        (breaks (string-breaks string)))
                   (flet ((test ()
                            (eval-in-context *context* (make-instance 'comparison-result
                                                                      :expression `(is equal (string-breaks ,string) ,breaks)
                                                                      :value-form `(string-breaks ,string)
                                                                      :expected expected
                                                                      :body (lambda () breaks)
                                                                      :comparison 'equal))))
                     (if (find i *skipped*)
                         (skip "Strange test" (test))
                         (test)))))))))
