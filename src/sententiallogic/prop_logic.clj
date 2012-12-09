(ns
    ^{:doc "Collection of constants capturing the standard logical
            sentential connectives as well as a collection of functions
            centered around propositional logic.

            Functions exist to determine whether a statement is a tautology,
            and for determine if a collection of statements are consistent
            or not.  These functions can be thought of as being the beginngings
            of a kernal for creating a language for dealing with the domain
            of mathematical logic."
      :author "Paul Evans"}
  sententiallogic.prop-logic
  (:use [sententiallogic.core])
  (:use [sententiallogic.reader-prop-logic]))

(defn make-interpretations
  "Returns a list of lists.  Each sub-list is a set of boolean values
   that represent a unique interpretation (of boolean values) of the
   argument set args-set.  The size of the list returned is equal
   to 2^(count args-set)"
  [args-set]
  (letfn [(left-pad-zeros [s num-digits]
            (loop [accumulator s]
              (if (= num-digits (count accumulator))
                accumulator
                (recur (str "0" accumulator)))))

          (make-bin-str-interp [n num-digits]
            (left-pad-zeros (Integer/toString n 2) num-digits))

          (to-bool-interp [bin-str-interp]
            (map #(= \1 %) bin-str-interp))]

    (let [num-args (count args-set)
          num-perms (exponentiate 2 num-args)]
      (if (= 0 num-args)
        nil
        (loop [accumulator '() n 0]
          (if (< n num-perms)
            (recur (cons (to-bool-interp (make-bin-str-interp n num-args))
                         accumulator) (inc n))
            accumulator))))))

(defn conjugate-exps
  "Returns an expression that is the conjugation of the expressions exps.  exps
   should be a list of compiled expressions."
  [exps]
  (letfn [(conjugate-exps [conjugation sen]
            (let [num-sen (count sen)]
              (if (= 1 num-sen)
                conjugation
                (recur (make-conjugation conjugation (second sen))
                       (rest sen)))))]
    (conjugate-exps (first exps) exps)))

(defmulti is-tautology?
  "Returns true if the expression exp is a tautology (i.e., yields
   a value of true for all interpretations of its arguments)"
  (fn [strategy _] strategy))

(defmethod
  ^{:doc "Implementation that uses a truth-table (crappy performance)"}
  is-tautology? :truth-table
  [_ exp]
  (let [all-interps (make-interpretations (args exp))]
    (every? #(apply (make-function (as-exp exp) (args exp)) %1)
            all-interps)))

(defmulti is-logical-conclusion?
  "Returns true if the premise expressions premise-exps tautologically
          imply the conclusion expression conclusion-exp (i.e., the conjugation
          of the premises implies the conclusion yielding a value of true for
          all interpretations of the arguments)."
  (fn [strategy _ & _] strategy))

(defmethod
  ^{:doc "Implementation that uses a truth-table (bad performance)"}
  is-logical-conclusion? :truth-table
  [_ conclusion-exp & premise-exps]
  (is-tautology?
   :truth-table
   (make-implication (conjugate-exps premise-exps) conclusion-exp)))

(defmulti consistent?
  "Returns true if the premises provided are consistent. 'consistent'
          means that there exists at least 1 interpretation of the premises
          such that their conjugation is true.  If no such interpretation
          exists, false is returned."
  (fn [strategy _ & _] strategy))

(defmethod
  ^{:doc "Implementation that uses a truth-table (bad performance)"}
  consistent? :truth-table
  [_ p1 & ps]
  (let [conjugation (conjugate-exps (cons p1 ps))
        conjugation-args (args conjugation)
        all-interps (make-interpretations conjugation-args)]
    (not (not-any? #(apply (make-function (as-exp conjugation)
                                          conjugation-args) %1)
                   all-interps))))

(defn implication
  "Returns the implication of p and q.  That is, returns the boolean
   result of p -> q as defined by classical logic"
  [p q]
  (if (not p)
    true
    q))

(defn equivalence
  "Returns the equivalence of p and q.  That is, returns the boolean
   result of p <-> q as defined by classical logic"
  [p q]
  (or (and p q)
      (and (not p) (not q))))
