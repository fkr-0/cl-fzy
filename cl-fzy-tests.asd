(in-package :asdf-user)
(defsystem "cl-fzy-tests"
  :description "Test suite for the cl-fzy system"
  :author "cbadger <cbadger@mail.com>"
  :version "0.0.1"
  :depends-on (:cl-fzy
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-cl-fzy")
                                     (:file "test-arr2d")
                                     (:file "test-choices")
                                     (:file "test-match")))))
                                     ;; (:file "choices"))))
                                     ;; (:file "tty")))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  
