(ns
    ^{:doc "Unit tests."
      :author "Paul Evans"}
    sententiallogic.reader-prop-logic-test
  (:use [sententiallogic.reader-prop-logic])
  (:use [clojure.test]))

(deftest
  ^{:doc "Tests for is-binary-oop?"}
  test-is-binary-op?
  (is (= true (is-binary-op? 'OR)))
  (is (= true (is-binary-op? 'AND)))
  (is (= true (is-binary-op? '->)))
  (is (= true (is-binary-op? '<->)))
  (is (= false (is-binary-op? 'NOT)))
  (is (= false (is-binary-op? 'a)))
  (is (= false (is-binary-op? '())))
  (is (= false (is-binary-op? nil)))
  (is (= false (is-binary-op? '(OR)))))

(deftest
  ^{:doc "Tests for is-unary-op?"}
  test-is-unary-op?
  (is (= true (is-unary-op? 'NOT)))
  (is (= false (is-unary-op? 'AND)))
  (is (= false (is-unary-op? 'OR)))
  (is (= false (is-unary-op? 'a)))
  (is (= false (is-unary-op? nil))))

(deftest
  ^{:doc "Tests for is-op?"}
  test-is-op?
  (is (= true (is-op? 'OR)))
  (is (= true (is-op? 'AND)))
  (is (= true (is-op? '->)))
  (is (= true (is-op? '<->)))
  (is (= true (is-op? 'NOT)))
  (is (= false (is-op? 'a)))
  (is (= false (is-op? '())))
  (is (= false (is-op? nil)))
  (is (= false (is-op? '(OR)))))

(deftest
  ^{:doc "Tests for to-binary-exp-syntax"}
  test-to-binary-exp-syntax
  (is (= '(or p q) (to-binary-exp-syntax 'p 'OR 'q)))
  (is (= '(and p q) (to-binary-exp-syntax 'p 'AND 'q)))
  (is (= (list implication-exp-opr 'p 'q)
         (to-binary-exp-syntax 'p '-> 'q)))
  (is (= (list equivalence-exp-opr 'p 'q)
         (to-binary-exp-syntax 'p '<-> 'q))))

(deftest
  ^{:doc "Tests for to-binary-sen-syntax"}
  test-to-binary-sen-syntax
  (is (= '(p OR q) (to-binary-sen-syntax 'or 'p 'q)))
  (is (= '(p AND q) (to-binary-sen-syntax 'and 'p 'q)))
  (is (= '(p <-> q) (to-binary-sen-syntax equivalence-exp-opr 'p 'q)))
  (is (= '(p -> q) (to-binary-sen-syntax implication-exp-opr 'p 'q))))

(deftest
  ^{:doc "Tests for to-unary-exp-syntax"}
  test-to-unary-exp-syntax
  (is (= '(not p) (to-unary-exp-syntax 'NOT 'p))))

(deftest
  ^{:doc "Tests for first-sub-exp"}
  test-first-sub-exp
  (is (= '(p OR q) (first-sub-exp '((p OR q) AND q))))
  (is (= 'p (first-sub-exp '(p OR q)))))

(deftest
  ^{:doc "Tests for second-sub-exp"}
  test-second-sub-exp
  (is (= '(NOT q) (second-sub-exp '(p AND (NOT q)))))
  (is (= 'p (second-sub-exp '((p AND q) OR p)))))

(deftest
  ^{:doc "Tests for operator"}
  test-operator
  (is (= '<-> (operator '(p <-> q))))
  (is (= 'OR (operator '(p OR q)))))

(deftest
  ^{:doc "Tests for unary-operator"}
  test-unary-operator
  (is (= 'NOT (unary-operator '(NOT (p OR q))))))

(deftest
  ^{:doc "Tests for compile-sentence using a valid sentence string"}
  test-v-compile-sentence-1
  (let [exp (compile-sentence '(p OR q))]
    (is (= true (is-valid exp)))
    (is (= '(or p q) (as-exp exp)))
    (is (= (set '(p q)) (args exp)))
    (is (= '(p OR q) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using a valid sentence string"}
  test-v-compile-sentence-2
  (let [exp (compile-sentence '(NOT q))]
    (is (= true (is-valid exp)))
    (is (= '(not q) (as-exp exp)))
    (is (= (set '(q)) (args exp)))
    (is (= '(NOT q) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using a valid sentence string"}
  test-v-compile-sentence-3
  (let [exp (compile-sentence '(NOT (p OR (NOT x))))]
    (is (= true (is-valid exp)))
    (is (= '(not (or p (not x))) (as-exp exp)))
    (is (= (set '(p x)) (args exp)))
    (is (= '(NOT (p OR (NOT x))) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using a valid sentence string"}
  test-v-compile-sentence-4
  (let [exp (compile-sentence '(z AND (p OR (NOT (NOT (NOT (a OR p)))))))]
    (is (= true (is-valid exp)))
    (is (= '(and z (or p (not (not (not (or a p)))))) (as-exp exp)))
    (is (= (set '(z p a)) (args exp)))
    (is (= '(z AND (p OR (NOT (NOT (NOT (a OR p)))))) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using a invalid sentence string"}
  test-inv-compile-sentence-1
  (let [exp (compile-sentence '())]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '() (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-2
  (let [exp (compile-sentence '(AND q))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(_ AND q) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-3
  (let [exp (compile-sentence '(NOT))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(NOT _) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-4
  (let [exp (compile-sentence '(p q))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(p _ q) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-5
  (let [exp (compile-sentence '(NOT NOT))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(NOT _) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-6
  (let [exp (compile-sentence '(NOT (a OR (NOT NOT))))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(NOT (a OR (NOT _))) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-7
  (let [exp (compile-sentence '(NOT (a OR (p q))))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(NOT (a OR (p _ q))) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-8
  (let [exp (compile-sentence '(OR AND OR))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(_ AND _) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-9
  (let [exp (compile-sentence '(NOT (NOT NOT)))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(NOT (NOT _)) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-10
  (let [exp (compile-sentence '(NOT (NOT (OR OR NOT))))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(NOT (NOT (_ OR _))) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-11
  (let [exp (compile-sentence '(NOT NOT NOT))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(_ _ _) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-12
  (let [exp (compile-sentence '(OR OR NOT))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(_ OR _) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-13
  (let [exp (compile-sentence '(OR NOT))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(_ OR _) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-14
  (let [exp (compile-sentence '(p AND))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '(p AND _) (as-sentence exp)))))

(deftest
  ^{:doc "Tests for compile-sentence using an invalid sentence string"}
  test-inv-compile-sentence-15
  (let [exp (compile-sentence '((p AND) OR (AND r)))]
    (is (= false (is-valid exp)))
    (is (= nil (as-exp exp)))
    (is (= nil (args exp)))
    (is (= '((p AND _) OR (_ AND r)) (as-sentence exp)))))
