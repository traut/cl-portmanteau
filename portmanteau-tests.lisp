(defpackage #:portmanteau-tests
  (:use :cl
        :portmanteau
        :fiveam)
  (:export #:all-tests))

(in-package #:portmanteau-tests)

(def-suite main-suite)
(in-suite main-suite)

(test too-short-test
  (is-false (portmanteau "cat" "attorney")))

(test matching-too-close-to-beginning-test
  (is-false (portmanteau "horse" "automobile")))

(test matching-too-close-to-end-test
  (is-false (portmanteau "cake" "apple")))

(test not-matching-test
  (is-false (portmanteau "laptop" "synthesizer")))

(test merger-test
  (is (equal "motel" (portmanteau "motor" "hotel")))
  (is (equal "labradoodle" (portmanteau "labrador" "poodle")))
  (is (equal "mocumentary" (portmanteau "mock" "documentary")))
  (is (equal "televangelist" (portmanteau "television" "evangelist"))))

(defun all-tests ()
  (run! 'main-suite))

;;; vim: set ft=lisp lisp:
