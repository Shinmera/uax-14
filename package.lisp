#|
 This file is a part of UAX-14
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.uax-14
  (:use #:cl)
  (:export
   #:*line-break-database-file*
   #:*pair-table-file*
   #:no-database-files
   #:load-databases
   #:compile-databases
   #:breaker
   #:make-breaker
   #:next-break
   #:list-breaks
   #:break-string))
