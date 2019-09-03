#|
 This file is a part of UAX-14
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem uax-14
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Implementation of the Unicode Standards Annex #14's line breaking algorithm"
  :homepage "https://github.com/Shinmera/uax-14"
  :bug-tracker "https://github.com/Shinmera/uax-14/issues"
  :source-control (:git "https://github.com/Shinmera/uax-14.git")
  :serial T
  :components ((:file "package")
               (:file "database")
               (:file "uax-14")
               (:file "documentation"))
  :depends-on (:documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :uax-14-test))))
