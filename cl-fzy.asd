(in-package :asdf-user)

(defsystem "cl-fzy"
  :author "cbadger <cbadger@systemli.org>"
  :version "0.0.1"
  :license "MIT"
  :description "reimplementation of fzy in cl"
  :homepage "https://"
  :bug-tracker ""
  :source-control (:git "")
  ;; Dependencies.

  ;; Project stucture.
  :serial t
  :components ((:module "./src"
                 :serial t
                 :components ((:file "packages")
                               (:file "cl-fzy")
                               (:file "arr")
                               (:file "match")
                               (:file "choices"))))
  ;; (:file "tty"))))


  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "cl-fzy"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "cl-fzy:main")
