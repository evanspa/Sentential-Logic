(ns
    ^{:doc "Collection of various helper funtions.  These functions are not
            particular to any domain."
      :author "Paul Evans"}
  sententiallogic.core)

(defn atom?
  "Returns true if l is not a list, sequence or collection"
  [l]
  (not (or (seq? l) (coll? l) (list? l))))

(defn exponentiate
  "Returns the value of raising n to the power of pow"
  [n pow]
  (loop [accumulator 1 pow pow]
    (if (= 0 pow)
      accumulator
      (recur (* accumulator n) (dec pow)))))

(defn some-by-regex
  "Each item in coll is assumed to have a regular expression pattern
   string defined on it somewhere.  regex-selector is a function for
   getting the regular expression pattern string from an element
   within coll.  This function thus returns the first item in coll
   in which some-str matches using the regular expression pattern
   fetched by regex-selector."
  [some-str regex-selector coll]
  (some (fn [item]
          (let [regex-pattern (re-pattern (regex-selector item))]
            (if (not (nil? (re-matches regex-pattern some-str)))
              item
              nil))) coll))

(defn is-match?
  "Returns true if the regular expression pattern string returned by
   regex-selector (invoked against item) matches with some-str."
  [some-str regex-selector item]
  (let [regex-pattern (re-pattern (regex-selector item))]
    (not (nil? (re-matches regex-pattern some-str)))))

(defn method-key
  "TODO: define this so that it can take any number of args, and its
   job is to wrap each arg in square brackets and return the final
   string."
  [format restapi-version]
  (str "[" format "][" restapi-version "]"))