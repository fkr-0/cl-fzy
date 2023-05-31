LISP ?= sbcl

all: test

run:
	# rlwrap $(LISP) --load run.lisp
	ros run --load ~/.sbclrc --load run.lisp

build:
	ros run --load ~/.sbclrc -- \
		--non-interactive \
		--load cl-fzy.asd \
		--eval '(ql:quickload :cl-fzy)' \
		--eval '(asdf:make :cl-fzy)'

test:
	ros run --non-interactive \
		--load ~/.sbclrc \
		--load run-tests.lisp

