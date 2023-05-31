
(load "cl-fzy.asd")
(load "cl-fzy-tests.asd")

(ql:quickload "cl-fzy-tests")

(in-package :cl-fzy-tests)

(uiop:quit (if (run-all-tests) 0 1))
