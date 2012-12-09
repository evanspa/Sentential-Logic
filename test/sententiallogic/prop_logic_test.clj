(ns
    ^{:doc "Unit tests."
      :author "Paul Evans"}
    sententiallogic.prop-logic-test
    (:use [sententiallogic.prop-logic])
    (:use [sententiallogic.reader-prop-logic
           :only [compile-sentence is-valid as-exp]])
    (:use [clojure.test]))

(deftest
  ^{:doc "Tests for implication"}
  test-implication
  (is (= true (implication true true)))
  (is (= false (implication true false)))
  (is (= true (implication false true)))
  (is (= true (implication false false))))

(deftest
  ^{:doc "Tests for equivalence"}
  test-equivalence
  (is (= true (equivalence true true)))
  (is (= false (equivalence true false)))
  (is (= false (equivalence false true)))
  (is (= true (equivalence false false))))

(deftest
  ^{:doc "Tests for make-interpretations"}
  test-make-interpretations-1
  (let [interps (make-interpretations (set '(a)))]
    (is (= 2 (count interps)))
    (is (= true (first (first interps))))
    (is (= false (first (second interps))))))

(deftest
  ^{:doc "Tests for make-interpretations"}
  test-make-interpretations-2
  (let [interps (make-interpretations (set '()))]
    (is (= nil interps))))

(deftest
  ^{:doc "Tests for make-interpretations"}
  test-make-interpretations-3
  (let [interps (make-interpretations (set '(a b)))]
    (is (= 4 (count interps)))
    (is (= true (first (first interps))))
    (is (= true (first (second interps))))
    (is (= false (first (nth interps 2))))
    (is (= false (first (nth interps 3))))
    (is (= true (second (first interps))))
    (is (= false (second (second interps))))
    (is (= true (second (nth interps 2))))
    (is (= false (second (nth interps 3))))))

(deftest
  ^{:doc "Tests for conjugate-exps"}
  test-conjugate-exps-1
  (let [conjugation (conjugate-exps
                     (list (compile-sentence 'p)
                           (compile-sentence 'q)))]
    (is (= true (is-valid conjugation)))
    (is (and (= 'and (first (as-exp conjugation)))
             (= 'p (second (as-exp conjugation)))
             (= 'q (nth (as-exp conjugation) 2))))))

(deftest
  ^{:doc "Tests for conjugate-exps"}
  test-conjugate-exps-2
  (let [conjugation (conjugate-exps
                     (list (compile-sentence '(p OR q))
                           (compile-sentence '(NOT r))
                           (compile-sentence 't)))]
    (is (= true (is-valid conjugation)))
    (is (and (= 'and (first (as-exp conjugation)))
             (= 'and (first (second (as-exp conjugation))))
             (= '(or p q) (second (second (as-exp conjugation))))
             (= '(not r) (nth (second (as-exp conjugation)) 2))
             (= 't (nth (as-exp conjugation) 2))))))

(deftest
  ^{:doc "Testing is-tautology? for basic sentences that are not tautologies."}
  test-is-tautology?-1
  (is (not (is-tautology? :truth-table (compile-sentence 'p))))
  (is (not (is-tautology? :truth-table (compile-sentence '(p AND q)))))
  (is (not (is-tautology? :truth-table (compile-sentence '(p OR q)))))
  (is (not (is-tautology? :truth-table (compile-sentence '(p -> q)))))
  (is (not (is-tautology? :truth-table (compile-sentence '(p <-> q))))))

(deftest
  ^{:doc "Testing: Law of Detachment"}
  test-LofD-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence
        '((p AND (p -> q)) -> q)))))

(deftest
  ^{:doc "Testing: Law of Detachment"}
  test-LofD-is-tautology?-2
  (is (is-tautology?
       :truth-table
       (compile-sentence
        '(((r OR z) AND ((r OR z) -> q)) -> q)))))

(deftest
  ^{:doc "Testing: Modus tollendo tollens"}
  test-Mtt-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence '(((NOT q) AND (p -> q)) -> (NOT p))))))

(deftest
  ^{:doc "Testing: Modus tollendo ponens"}
  test-Mtp-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence '(((NOT p) AND (p OR q)) -> q)))))

(deftest
  ^{:doc "Testing: Law of Simplication"}
  test-LofS-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence '((p AND q) -> p)))))

(deftest
  ^{:doc "Testing: Law of Adjunction"}
  test-LofA-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence '((p AND q) -> (p AND q))))))

(deftest
  ^{:doc "Testing: Law of Hypothetical Syllogism"}
  test-LofHS-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence '(((p -> q) AND (q -> r)) -> (p -> r))))))

(deftest
  ^{:doc "Testing: Law of Exportation"}
  test-LofExp-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence
        '(((p AND q) -> r) -> (p -> (q -> r)))))))

(deftest
  ^{:doc "Testing: Law of Importation"}
  test-LofImp-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence
        '((p -> (q -> r)) -> ((p AND q) -> r))))))

(deftest
  ^{:doc "Testing: Law of Absurdity"}
  test-LofAbs-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence '((p -> (q AND (NOT q))) -> (NOT p))))))

(deftest
  ^{:doc "Testing Law of (I forget)"}
  test-LofAdd-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence '(p -> (p OR q))))))

(deftest
  ^{:doc "Testing: Law of Double Negation"}
  test-LofDN-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence '(p <-> (NOT (NOT p)))))))

(deftest
  ^{:doc "Testing: Law of Contraposition"}
  test-LofContra-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence '((p -> q) <-> ((NOT q) -> (NOT p)))))))

(deftest
  ^{:doc "Testing: DeMorgan's Laws"}
  test-DeMorg-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence '((NOT (p AND q)) <-> ((NOT p) OR (NOT q)))))))

(deftest
  ^{:doc "Testing: DeMorgan's Laws"}
  test-DeMorg-is-tautology?-2
  (is (is-tautology?
       :truth-table
       (compile-sentence '((NOT (p OR q)) <-> ((NOT p) AND (NOT q)))))))

(deftest
  ^{:doc "Testing: Commutative Laws"}
  test-comm-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence '((p AND q) <-> (q AND p))))))

(deftest
  ^{:doc "Testing: Commutative Laws"}
  test-comm-is-tautology?-2
  (is (is-tautology?
       :truth-table
       (compile-sentence '((p OR q) <-> (q OR p))))))

(deftest
  ^{:doc "Testing: Law of Equivalence for Implication and Disjunction"}
  test-LofEqImpDis-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence
        '((p -> q) <-> ((NOT p) OR q))))))

(deftest
  ^{:doc "Testing: Law of Negation for Implication"}
  test-LofNegImp-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence
        '((NOT (p -> q)) <-> (p AND (NOT q)))))))

(deftest
  ^{:doc "Testing: Law for Biconditional Sentences"}
  test-LforBicondSen-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence
        '((p <-> q) <-> ((p -> q) AND (q -> p)))))))

(deftest
  ^{:doc "Testing: Law for Biconditional Sentences"}
  test-LforBicondSen-is-tautology?-2
  (is (is-tautology?
       :truth-table
       (compile-sentence
        '((p <-> q) <-> ((p AND q) OR ((NOT p) AND (NOT q))))))))

(deftest
  ^{:doc "Testing: Law of Excluded Middle"}
  test-LofExcMid-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence '(p OR (NOT p))))))

(deftest
  ^{:doc "Testing: Law of Contradiction"}
  test-LofContrad-is-tautology?-1
  (is (is-tautology?
       :truth-table
       (compile-sentence
        '(NOT (p AND (NOT p)))))))

(deftest
  ^{:doc "Testing is-logical-conclusion?"}
  test-is-logical-conclusion?-1
  (is (is-logical-conclusion? :truth-table
       (compile-sentence 'q)
       (compile-sentence 'p)
       (compile-sentence '(p -> q)))))

(deftest
  ^{:doc "Testing is-logical-conclusion?"}
  test-is-logical-conclusion?-2
  (is (not (is-logical-conclusion? :truth-table
       (compile-sentence 'q)
       (compile-sentence '(NOT p))
       (compile-sentence '(p -> q))))))

(deftest
  ^{:doc "Testing consistent?"}
  test-consistent?-1
  (is (consistent? :truth-table
       (compile-sentence 'p)
       (compile-sentence 'q)
       (compile-sentence '(p -> q)))))

(deftest
  ^{:doc "Testing consistent?"}
  test-consistent?-2
  (is (not (consistent? :truth-table
       (compile-sentence 'p)
       (compile-sentence 'q)
       (compile-sentence '(NOT (p -> q)))
       (compile-sentence '(p -> q))))))

(deftest
  ^{:doc "Testing consistent?"}
  test-consistent?-3
  (is (consistent? :truth-table
       (compile-sentence 'p)
       (compile-sentence 'q)
       (compile-sentence '(p AND q))
       (compile-sentence '(p -> q)))))
