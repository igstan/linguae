# Pattern Match Compiler

A reconstruction of the pattern match compiler presented in [ML pattern match
compilation and partial evaluation][paper].

## Version 1: Naive Matcher

```
- CM.make "lib.cm"; NaiveMatcher.test ();
val it = true : bool
val it = SOME 2 : int option
```

## Version 2: Instrumented Matcher

```
- CM.make "lib.cm"; InstrumentedMatcher.test ();
val it = true : bool
val it = SOME 2 : int option
```

## Version 3: Match Compiler

```
- CM.make "lib.cm"; MatchCompiler.test ();
val it = true : bool

WHEN: Obj.1 = True
THEN:
  WHEN: Obj.2 = Green
  THEN: SUCCESS: 1
  ELSE: FAILURE
ELSE:
  WHEN: Obj.2 = Green
  THEN: SUCCESS: 2
  ELSE:
    WHEN: Obj.2 = Red
    THEN: SUCCESS: 3
    ELSE: SUCCESS: 4

WHEN: Obj.1 = True
THEN:
  WHEN: Obj.2 = Green
  THEN: SUCCESS: 1
  ELSE:
    WHEN: Obj.2 = Blue
    THEN: SUCCESS: 4
    ELSE: FAILURE
ELSE:
  WHEN: Obj.2 = Green
  THEN: SUCCESS: 2
  ELSE:
    WHEN: Obj.2 = Red
    THEN: SUCCESS: 3
    ELSE: FAILURE
val it = () : unit
```

## Notes

The code uses Successor ML features, such as leading vertical bars and
single line comments.

Code was run using these SML/NJ options, of which the mandatory one is just
`-Cparser.succ-ml=true`:

```
sml \
  -Cprint.length=100 \
  -Cprint.depth=20 \
  -Ccontrol.poly-eq-warn=false \
  -Cprint.signatures=2 \
  -Cparser.succ-ml=true \
  -Cparser.secondary-prompt='… ' \
  -Cparser.quotations=true \
  -Ctdp.instrument=true \
  '$smlnj-tdp/back-trace.cm'
```

[paper]: ./doc/ml-pattern-match-compilation-and-partial-evaluation.pdf
