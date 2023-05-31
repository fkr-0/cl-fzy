(in-package :cl-fzy-tests)

(def-suite test-match :description "Test the match function again")

(in-suite test-match)

(defvar +score-tolerance+ 0.000001)

(defun is-score-eq (a b)
  (is (< (abs (- a b))  +score-tolerance+)))

(defun is-size-t-eq (a b)
  (is (= (abs (- a b)) 0)))

(test exact-match-should-return-true
  (is (cl-fzy::has-match "a" "a")))

(test partial-match-should-return-true
  (is (cl-fzy::has-match "a" "ab"))
  (is (cl-fzy::has-match "a" "ba")))

(test match-with-delimiters-in-between
  (is (cl-fzy::has-match "abc" "a|b|c")))

(test non-match-should-return-false
  (is (not (cl-fzy::has-match "a" "")))
  (is (not (cl-fzy::has-match "a" "b")))
  (is (not (cl-fzy::has-match "ass" "tags"))))

(test empty-query-should-always-match
  (is (cl-fzy::has-match "" ""))
  (is (cl-fzy::has-match "" "a")))

(test should-prefer-starts-of-words
  (is (> (cl-fzy::match "amor" "app/models/order") (cl-fzy::match "amor" "app/models/zrder"))))

(test should-prefer-consecutive-letters
  (is (< (cl-fzy::match "amo" "app/m/foo") (cl-fzy::match "amo" "app/models/foo"))))

(test should-prefer-contiguous-over-letter-following-period
  (is (< (cl-fzy::match "gemfil" "Gemfile.lock") (cl-fzy::match "gemfil" "Gemfile"))))

(test should-prefer-shorter-matches
  (is (> (cl-fzy::match "abce" "abcdef") (cl-fzy::match "abce" "abc de")))
  (is (> (cl-fzy::match "abc" "    a b c ") (cl-fzy::match "abc" " a  b  c ")))
  (is (> (cl-fzy::match "abc" " a b c    ") (cl-fzy::match "abc" " a  b  c "))))

(test should-prefer-shorter-candidates
  (is (> (cl-fzy::match "test" "tests") (cl-fzy::match "test" "testing"))))

(test should-prefer-start-of-candidate
  (is (> (cl-fzy::match "test" "testing") (cl-fzy::match "test" "/testing"))))

(test score-exact-match
  (is-score-eq cl-fzy::+score-max+ (cl-fzy::match "abc" "abc"))
  (is-score-eq cl-fzy::+score-max+ (cl-fzy::match "aBc" "abC")))

(test score-empty-query
  (is-score-eq cl-fzy::+score-min+ (cl-fzy::match "" ""))
  (is-score-eq cl-fzy::+score-min+ (cl-fzy::match "" "a"))
  (is-score-eq cl-fzy::+score-min+ (cl-fzy::match "" "bb")))

(test score-gaps
  (is-score-eq cl-fzy::+score-gap-leading+ (cl-fzy::match "a" "*a"))
  (is-score-eq (* cl-fzy::+score-gap-leading+ 2) (cl-fzy::match "a" "*ba"))
  (is-score-eq (+ (* cl-fzy::+score-gap-leading+ 2) cl-fzy::+score-gap-trailing+) (cl-fzy::match "a" "**a*"))
  (is-score-eq (+ (* cl-fzy::+score-gap-leading+ 2) (* cl-fzy::+score-gap-trailing+ 2)) (cl-fzy::match "a" "**a**"))
  (is-score-eq (+ (* cl-fzy::+score-gap-leading+ 2) cl-fzy::+score-match-consecutive+ (* cl-fzy::+score-gap-trailing+ 2)) (cl-fzy::match "aa" "**aa**"))
  (is-score-eq (+ cl-fzy::+score-gap-leading+ cl-fzy::+score-gap-leading+ cl-fzy::+score-gap-inner+ cl-fzy::+score-gap-trailing+ cl-fzy::+score-gap-trailing+) (cl-fzy::match "aa" "**a*a**")))

(test score-consecutive
  (is-score-eq (+ cl-fzy::+score-gap-leading+ cl-fzy::+score-match-consecutive+) (cl-fzy::match "aa" "*aa"))
  (is-score-eq (+ cl-fzy::+score-gap-leading+ (* cl-fzy::+score-match-consecutive+ 2)) (cl-fzy::match "aaa" "*aaa"))
  (is-score-eq (+ cl-fzy::+score-gap-leading+ cl-fzy::+score-gap-inner+ cl-fzy::+score-match-consecutive+) (cl-fzy::match "aaa" "*a*aa")))

(test score-slash
  (is-score-eq (+ cl-fzy::+score-gap-leading+ cl-fzy::+score-match-slash+) (cl-fzy::match "a" "/a"))
  (is-score-eq (+ (* cl-fzy::+score-gap-leading+ 2) cl-fzy::+score-match-slash+) (cl-fzy::match "a" "*/a"))
  (is-score-eq (+ (* cl-fzy::+score-gap-leading+ 2) cl-fzy::+score-match-slash+ cl-fzy::+score-match-consecutive+) (cl-fzy::match "aa" "a/aa")))

(test score-capital
  (is-score-eq (+ cl-fzy::+score-gap-leading+ cl-fzy::+score-match-capital+) (cl-fzy::match "a" "bA"))
  (is-score-eq (+ (* cl-fzy::+score-gap-leading+ 2) cl-fzy::+score-match-capital+) (cl-fzy::match "a" "baA"))
  (is-score-eq (+ (* cl-fzy::+score-gap-leading+ 2) cl-fzy::+score-match-capital+ cl-fzy::+score-match-consecutive+) (cl-fzy::match "aa" "baAa")))

(test score-dot
  (is-score-eq (+ cl-fzy::+score-gap-leading+ cl-fzy::+score-match-dot+) (cl-fzy::match "a" ".a"))
  (is-score-eq (+ (* cl-fzy::+score-gap-leading+ 3) cl-fzy::+score-match-dot+) (cl-fzy::match "a" "*a.a"))
  (is-score-eq (+ cl-fzy::+score-gap-leading+ cl-fzy::+score-gap-inner+ cl-fzy::+score-match-dot+) (cl-fzy::match "a" "*a.a")))

(test score-long-string
  (let ((string (make-string 4096 :initial-element #\a)))
    (setf (aref string 4095) #\null)
    (is-score-eq cl-fzy::+score-min+ (cl-fzy::match "aa" string))
    (is-score-eq cl-fzy::+score-min+ (cl-fzy::match string "aa"))
    (is-score-eq cl-fzy::+score-min+ (cl-fzy::match string string))))


(test positions-consecutive
  (let ((positions (make-array 3)))
    (cl-fzy::match-positions "amo" "app/models/foo" positions)
    (is (= 0 (aref positions 0)))
    (is (= 4 (aref positions 1)))
    (is (= 5 (aref positions 2)))))

(test positions-start-of-word
  (let ((positions (make-array 4 :initial-element 0)))
    (cl-fzy::match-positions "amor" "app/models/order" positions)
    (is (= 0 (aref positions 0)))
    (is (= 4 (aref positions 1)))
    (is (= 11 (aref positions 2)))
    (is (= 12 (aref positions 3)))))

(test positions_multiple_candidates_start_of_words
  (let ((positions (make-array 3 :initial-element 0)))
    (cl-fzy::match-positions "abc" "a/a/b/c/c" positions)
    (is (= 2 (aref positions 0)))
    (is (= 4 (aref positions 1)))
    (is (= 6 (aref positions 2)))))

(test positions_exact_match
  (let ((positions (make-array 3 :initial-element 0)))
    (cl-fzy::match-positions "foo" "foo" positions)
    (is (= 0 (aref positions 0)))
    (is (= 1 (aref positions 1)))
    (is (= 2 (aref positions 2)))))

;; (test positions_exact_match_start_of_word
;;   (let ((positions (make-array 3 :initial-element 0)))
;;     (cl-fzy::match-positions "foo" "foo/bar" positions)
;;     (format t "~a~%" positions)
;;     (is (= 0 (aref positions 0)))
;;     (is (= 1 (aref positions 1)))
;;     (is (= 2 (aref positions 2)))))

;; (test positions_exact_match_start_of_word_multiple_candidates
;;   (let ((positions (make-array 3 :initial-element 0)))
;;     (cl-fzy::match-positions "foo" "foo/bar/baz" positions)
;;     (format t "~a~%" positions)
;;     (is (= 0 (aref positions 0)))
;;     (is (= 1 (aref positions 1)))
;;     (is (= 2 (aref positions 2)))))

(test positions_no_bonuses
  (let ((positions (make-array 2 :initial-element 0)))
    (cl-fzy::match-positions "as" "tags" positions)
    (is (= 1 (aref positions 0)))
    (is (= 3 (aref positions 1)))))


(test positions_no_bonuses2
  (let ((positions (make-array 2 :initial-element 0)))
    (cl-fzy::match-positions "as" "examples.txt" positions)
    (is (= 2 (aref positions 0)))
    (is (= 7 (aref positions 1)))))
