(ns
    ^{:doc "Collection of functions that enable the compiling of statements of
            propositional logic."
      :author "Paul Evans"}
    sententiallogic.reader-prop-logic
  (:use [sententiallogic.core])
  (:use [clojure.set])
  (:require [clojure.string]))

(def
  ^:const
  ^{:doc "Constant representing the NOT sentential connective that would be
          used by a user creating a sentence in logic to be compiled."}
  not-sentence-opr 'NOT)

(def
  ^:const
  ^{:doc "Constant representing the AND (conjugatation) sentential connective
          that would be used by a user creating a sentence in logic to be
          compiled"}
  and-sentence-opr 'AND)

(def
  ^:const
  ^{:doc "Constant representing the OR (disjunction) sentential connective
          that would be used by a user creating a sentence in logic to be
          compiled."}
  or-sentence-opr 'OR)

(def
  ^:const
  ^{:doc "Constant representing the material implication sentential
          connective that would be used by a user creating a
          sentence in logic to be compiled."}
  implication-sentence-opr '->)

(def
  ^:const
  ^{:doc "Constant representing the equivalence sentential connective
          that would be used by a user creating a sentence in logic
          to be compiled."}
  equivalence-sentence-opr '<->)

(def
  ^:const
  ^{:doc "Constant representing the NOT sentential
          connective that would be used to construct a Clojure by
          compiling a user-entered sentence."}
  not-exp-opr 'not)

(def
  ^:const
  ^{:doc "Constant representing the AND sentential
          connective that would be used to construct a Clojure by
          compiling a user-entered sentence."}
  and-exp-opr 'and)

(def
  ^:const
  ^{:doc "Constant representing the OR sentential
          connective that would be used to construct a Clojure by
          compiling a user-entered sentence."}
  or-exp-opr 'or)

(def
  ^:const
  ^{:doc "Constant representing the material implication sentential
          connective that would be used to construct a Clojure by
          compiling a user-entered sentence."}
  implication-exp-opr
  "sententiallogic.prop-logic/implication")

(def
  ^:const
  ^{:doc "Constant representing the equivalence sentential
          connective that would be used to construct a Clojure by
          compiling a user-entered sentence."}
  equivalence-exp-opr
  "sententiallogic.prop-logic/equivalence")

(defn exp-op
  "Returns the corresponding expression operator for the given
   sentence operator sen-op."
  [sen-op]
  (if (= or-sentence-opr sen-op)
    or-exp-opr
    (if (= and-sentence-opr sen-op)
      and-exp-opr
      (if (= implication-sentence-opr sen-op)
        implication-exp-opr
        (if (= equivalence-sentence-opr sen-op)
          equivalence-exp-opr
          (if (= not-sentence-opr sen-op)
            'not
            nil))))))

(defn sen-op
  "Returns the corresponding sentence operator for the given
   expression operator exp-op."
  [exp-op]
  (if (= or-exp-opr exp-op)
    or-sentence-opr
    (if (= and-exp-opr exp-op)
      and-sentence-opr
      (if (= implication-exp-opr exp-op)
        implication-sentence-opr
        (if (= equivalence-exp-opr exp-op)
          equivalence-sentence-opr
          (if (= 'not exp-op)
            not-sentence-opr
            nil))))))

(defn is-binary-op?
  "Returns true if the string str is one of the binary sentential connectives:
   OR, AND, -> or <->"
  [str]
  (and (atom? str)
       (and (not (nil? (exp-op str)))
            (not (= str not-sentence-opr)))))

(defn is-unary-op?
  "Returns true if the string str is a unary operator: NOT"
  [str]
  (and (atom? str)
       (= str not-sentence-opr)))

(defn is-op?
  "Returns true if the string str is either a unary operator
   or binary sentential connective"
  [str]
  (or (is-unary-op? str)
      (is-binary-op? str)))

(defn to-binary-exp-syntax
  "Returns a binary expression from sen1 and sen2 and op.  The returned
   string is a valid Clojure boolean expression"
  [sen1 op sen2]
  (let [expr-op (exp-op op)]
    (if (nil? expr-op)
      nil
      (list expr-op sen1 sen2))))

(defn to-binary-sen-syntax
  "Returns a binary sentence of the form exp1 op exp2 given
   op and Clojure expressions exp1 and exp2"
  [op exp1 exp2]
  (let [sen-opr (sen-op op)]
    (if (nil? sen-opr)
      nil
      (list exp1 sen-opr exp2))))

(defn to-unary-exp-syntax
  "Returns a sentence with the unary operator op prefixed
   on it given the Clojure expression exp1"
  [op exp1]
  (let [expr-op (exp-op op)]
    (if (nil? expr-op)
      nil
      (list expr-op exp1))))

(defn first-sub-exp
  "Returns the first sub-expression of the binary sentence bsen"
  [bsen]
  (first bsen))

(defn second-sub-exp
  "Returns the second sub-expression of the binary sentence bsen"
  [bsen]
  (first (next (next bsen))))

(defn operator
  "Returns the binary operator (aka sentential connective) of bsen"
  [bsen]
  (first (next bsen)))

(defn unary-operator
  "Returns the unary operator of bsen"
  [bsen]
  (first bsen))

(defmulti compile-sentence
  "Attempts to 'compile' sentence.  sentence is meant to be a list
   of component sentence elements.

   Returns a list of 4 items:
    1. A boolean indicator - this indicates if sentence was compiled
       successfully or not
    2. If sentence is valid, then a Clojure expression representing
       the sentence is returned; to otherwise nil
    3. If sentence is valid, then a reproduction of sentence; otherwise
       a partial reproduction of sentence.  That is, a reproduction of
       sentence with hints to attempt to convey what was wrong with the
       syntax of sentence.  The hints manifest as '_' characters attempting
       to indicate an expected sentence or operator.
    4. A set that is the union of unique variables that appear in sentence"
  class)

(defmethod
  ^{:doc "Implementation that expects to receive a list as input."}
  compile-sentence :default [sentence]
  (letfn [(tl-collector [is-valid compiled-sen
                         reproduced-sen variable-map]
            (list is-valid (if is-valid
                             compiled-sen
                             nil) reproduced-sen (if is-valid
                                                   variable-map
                                                   nil)))
          (compile-sentence [sen k]
            (if (atom? sen)
              (if (not (is-op? sen))
                (trampoline #(k true sen sen (conj #{} sen)))
                (trampoline #(k false nil '_ nil)))
              (if (empty? sen)
                (k false nil sen nil)
                (if (= 1 (count sen))
                  (let [fsen (first sen)]
                    (if (not (is-op? fsen))
                      (trampoline #(k true sen sen (conj #{} fsen)))
                      (if (is-unary-op? fsen)
                        (trampoline #(k false nil (list fsen '_) nil))
                        (trampoline #(k false nil (list '_ fsen '_) nil)))))
                  (if (= 2 (count sen))
                    (let [fsen (first sen) ssen (second sen)]
                      (if (atom? fsen)
                        (if (is-unary-op? fsen)
                          (if (not (is-unary-op? ssen))
                            (recur ssen (fn [isv cs rs vm]
                                        #(k (and true isv)
                                            (to-unary-exp-syntax fsen cs)
                                            (list fsen rs) vm)))
                            (recur ssen (fn [isv cs rs vm]
                                          #(k false
                                              nil
                                              (list 'NOT '_)
                                              nil))))
                          (if (is-binary-op? fsen)
                            (recur ssen (fn [isv cs rs vm]
                                          #(k false
                                              nil
                                              (list '_ fsen rs)
                                              nil)))
                            (if (is-binary-op? ssen)
                              (recur ssen (fn [isv cs rs vm]
                                          #(k false
                                              nil
                                              (list fsen ssen '_)
                                              nil)))
                              (recur ssen (fn [isv cs rs vm]
                                          #(k false nil
                                              (list fsen '_ rs)
                                              nil))))))
                        (if (is-binary-op? ssen)
                          (recur fsen (fn [fisv fcs frs fvm]
                                        #(compile-sentence
                                          (first-sub-exp sen)
                                          (fn [sisv scs srs svm]
                                            (k false
                                               nil
                                               (list frs ssen '_)
                                               nil)))))
                          (recur fsen (fn [isv cs rs vm]
                                        #(k (and false isv)
                                            nil
                                            (list fsen '_ rs) nil))))))
                    (if (= 3 (count sen))
                      (if (is-binary-op? (operator sen))
                        (recur (first-sub-exp sen)
                               (fn [fisv fcs frs fvm]
                                 #(compile-sentence
                                   (second-sub-exp sen)
                                   (fn [sisv scs srs svm]
                                     (k (and fisv sisv)
                                        (to-binary-exp-syntax
                                         fcs (operator sen) scs)
                                        (list frs (operator sen) srs)
                                        (union fvm svm))))))
                        (recur (first-sub-exp sen)
                               (fn [fisv fcs frs fvm]
                                 #(compile-sentence
                                   (second-sub-exp sen)
                                   (fn [sisv scs srs svm]
                                     (k false nil (list frs '_ srs) nil))))))
                      (trampoline #(k false nil (list '_ '_ '_) nil))))))))]
    (compile-sentence sentence tl-collector)))

(defmethod
  ^{:doc "Implementation that expects to receive a string as input.  Then,
          the string is converted to a list and the companion version of
          this function is invoked."}
  compile-sentence String [sentence-as-str]
  (compile-sentence (load-string (str "'" sentence-as-str))))

(defn is-valid
  "Returns the is-valid indicator of the compiled expresson cexp"
  [cexp]
  (first cexp))

(defn as-exp
  "Returns the Clojure expression of the compiled expression cexp"
  [cexp]
  (second cexp))

(defn as-sentence
  "Returns the reproduced sentence of the compiled expression cexp"
  [cexp]
  (nth cexp 2))

(defn args
  "Returns the argument-set of the compiled expression cexp"
  [cexp]
  (nth cexp 3))

(defn make-binary-exp
  "Returns a compiled expression that is the combination of exp1 and
   exp2 connected by op"
  [op exp1 exp2]
  (let [sen-opr op exp-opr (exp-op op)]
    (list
     (and (is-valid exp1) (is-valid exp2))
     (to-binary-exp-syntax (as-exp exp1) sen-opr (as-exp exp2))
     (to-binary-sen-syntax exp-opr (as-sentence exp1) (as-sentence exp2))
     (union (args exp1) (args exp2)))))

(defn make-conjugation
  "Returns a compiled expression that is the conjugation of exp1 and exp2"
  [exp1 exp2]
  (make-binary-exp and-sentence-opr exp1 exp2))

(defn make-implication
  "Returns a compiled expression that is the implication of exp1 and exp2"
  [exp1 exp2]
  (make-binary-exp implication-sentence-opr exp1 exp2))

(defn make-function
  "Returns an actual Clojure function from the compiled expression cexp"
  [exp args]
  (load-string (str "(fn "
                    (str (vec args)
                         " "
                         (clojure.string/replace exp "\"" "") ")"))))
