(asdf:defsystem #:wireworld
  :version "0.1"
  :description "Brian Silverman's Wireworld simulator"
  :author "Stefano Rodighiero <stefano.rodighiero@gmail.com"
  :license "MIT"
  :serial t
  :depends-on (#:lispbuilder-sdl)
  :components ((:file "package")
               (:file "wireworld")))
