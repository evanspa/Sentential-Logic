# Sentential Logic
The sentential logic (aka propositional logic) is the simplest, and often first area broached when exploring Mathematical Logic.  Propositional logic consists of simple statements, or _premises_.  There are no quantified variables in propositional logic, and there are no user-defined predicates.  There are simply premises and the familiar sentential connectives:
+ OR, AND, If/Then (->), NOT, If and only If (<->)

## Warning
This project is a bit of a toy project of mine for exploring Clojure, and for playing with some concepts from classical logic.  The code for compiling a sentence is very crude, and should not be looked at as a proper way for parsing / processing strings.  A better way would be to create a lexer and use the lexer to parse input strings into tokens.  But such an implementation will have to wait.

## Examples

### Tautology
The concept of a tautology is foundational to logic.  Tautological implications (e.g., syllogism) and equivalences (e.g., DeMorgan's Laws) are building blocks of inference.  You can try to see if a given sentence is a tautology as follows:

```clojure
; Modus Ponens
(is-tautology? (compile-sentence '(((NOT p) AND (p OR q)) -> q)))

; One of DeMorgan's Laws
(is-tautology? (compile-sentence '((NOT (p AND q)) <-> ((NOT p) OR (NOT q)))))
```
As you can see, some symbols have been defined for the logic sentential connectives for implication and equivalence.  

The function `compile-sentence` can be used to analyze a given sentence for syntactical correctness.  A data structure is returned that encapsulates 4 things: 

1. a boolean indicating if the provided sentence was syntactically valid or not
2. your sentence converted to a Clojure form
3. your sentence repeated back
4. a set of the unique atomic premises that make up the sentence

```clojure
(compile-sentence '(NOT (NOT (p AND q))

=> (true (not (not (and p q))) (NOT (NOT (p AND q))) #{q p})
```

If you provide an invalid sentence, the data structure is returned will tell you so, along with a hint as to what is wrong with your input.

```clojure
; Invalid sentence
(compile-sentence '(p OR))

=> (false nil (p _ _) nil)

; Another invalid sentence
(compile-sentence '(p q)

=> (false nil (p _ q) nil)
```
The 3rd element of the return data structure is a sentence containing underscores attempting to hint where you went afoul.

## More Tautologies
Modus Ponens and DeMorgan were shown above.  There are of course several other useful tautological implications that are used when attempting to logically derive (deduce) a statement from a given set of premises.  

### Law of Detachment
```clojure
(is-tautology? (compile-sentence '((p AND (p -> q)) -> q)))

=> true 

(is-tautology? (compile-sentence '(((r OR z) AND ((r OR z) -> q)) -> q)))

=> true
```

### Law of Hypothetical Syllogism
```clojure
(is-tautology?
       (compile-sentence '(((p -> q) AND (q -> r)) -> (p -> r))))

=> true
```

There are many other common tautological implications and equivalences.  Take a look at the unit tests (in mathcomptools.test.logic.propositional) for more examples.

## Consistency of a set of Premises
Consistency of a given set of premises is a useful thing to be able to determine.  A set of premises are said to be consistent if the conjugation of them is true for at least 1 interpretation of the premises.
```clojure
(consistent?
       (compile-sentence 'p)
       (compile-sentence 'q)
       (compile-sentence '(p -> q)))

=> true

(consistent?
       (compile-sentence 'p)
       (compile-sentence 'q)
       (compile-sentence '(NOT (p -> q)))
       (compile-sentence '(p -> q)))

=> false
```

## External Resources
+ [Introduction to Logic](http://amzn.com/0486406873), by Patrick Suppes
+ [Propositional Logic at Wikipedia](http://en.wikipedia.org/wiki/Propositional_logic)
