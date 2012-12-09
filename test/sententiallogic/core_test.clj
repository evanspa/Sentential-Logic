(ns
    ^{:doc "Unit tests."
      :author "Paul Evans"}
    sententiallogic.core-test
  (:use [sententiallogic.core])
  (:use [clojure.test]))

(deftest
  ^{:doc "Tests for atom?"}
  test-atom?
  (is (= true (atom? 'a)))
  (is (= false (atom? '())))
  (is (= true (atom? 'abc)))
  (is (= true (atom? nil))))
