# A Toy Interpreter

```
> test
[info] InterpreterTest:
[info] - evaluates numbers
[info] - evaluates addition expressions
[info] - evaluates subtraction expressions
[info] - evaluates multiplication expressions
[info] - evaluates division expressions
[info] - evaluates if expressions: false branch
[info] - evaluates if expressions: true branch
[info] - evaluates function expressions
[info] - rejects non-numbers as left operands
[info] - rejects non-numbers as right operands
[info] - rejects non-numbers in condition position in if expressions
[info] - evaluates function application
[info] - rejects non-functions in function application position
[info] - evaluates parameter references
[info] - complains about unbound identifiers
[info] - evaluates let expressions
[info] - supports closures
[info] - functions introduce static, or lexical scope, not dynamic scope
[info] - evaluates two expressions in sequence
[info] - evaluates identifier update expressions
[info] - ensure scope does not leak
[info] - rejects update of an unbound identifier
[info] - evaluates recursive let expressions
[info] Run completed in 175 milliseconds.
[info] Total number of tests run: 23
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 23, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[success] Total time: 1 s, completed Dec 20, 2014 3:26:49 PM
```
