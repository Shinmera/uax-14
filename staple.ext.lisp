(asdf:load-system :staple-markless)

(defpackage "uax-14-docs"
  (:use #:cl)
  (:local-nicknames
   (#:uax-14 #:org.shirakumo.alloy.uax-14)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "uax-14-docs")))

(defmethod staple:subsystems ((system (eql (asdf:find-system :uax-14))))
  ())

(defmethod staple:page-type ((system (eql (asdf:find-system :uax-14))))
  'page*)

