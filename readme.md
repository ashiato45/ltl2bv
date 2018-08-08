## LTL2BA

Generates a Beuchi automaton that is equivalent to a given LTL formula.
The algorithm is based on the paper.
```
Vardi M.Y. (1996) An automata-theoretic approach to linear temporal logic. In: Moller F., Birtwistle G. (eds) Logics for Concurrency. Lecture Notes in Computer Science, vol 1043. Springer, Berlin, Heidelberg
```

### Requirements
- opam
- jbuilder
- janestreet/core

### Usage
Build and run by 
```
jbuilder exec app/ltl2ba.exe -- "(formula)"
````

It accepts a formula of the form:
- "p0", "p1", "p2",... / Atomic formulae
- "α β &" / Conjunction of formulae α and β
- "α !" / Negation of a formula α
- "α X" / Next of a formula α
- "α β U" / Until αUβ
