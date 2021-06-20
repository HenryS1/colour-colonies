(defpackage colour-colonies/tests/main
  (:use :cl
        :colour-colonies
        :rove))
(in-package :colour-colonies/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :colour-colonies)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
