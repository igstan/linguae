# A Simple Bytecode Virtual Machine

Follows Terence Parr's talk: [How to Build a Virtual Machine][video]. The
material is ultimately based on Chapter 10 of his book: [Language Implementation
Patterns][book].

## Trace for `factorial 3`

```
$ sml
- CM.make "lib.cm"; Main.main ();
val it = true : bool
0022: iconst 3      │ stack: [3]                                                        │ sp:  0; fp:  0; ip: 23
0024: call   0 1    │ stack: [3, 1, 0, 26]                                              │ sp:  3; fp:  3; ip: ~1
0000: load   ~3     │ stack: [3, 1, 0, 26, 3]                                           │ sp:  4; fp:  3; ip:  1
0002: iconst 2      │ stack: [3, 1, 0, 26, 3, 2]                                        │ sp:  5; fp:  3; ip:  3
0004: ilt           │ stack: [3, 1, 0, 26, 0]                                           │ sp:  4; fp:  3; ip:  4
0005: brf    10     │ stack: [3, 1, 0, 26]                                              │ sp:  3; fp:  3; ip:  9
0010: load   ~3     │ stack: [3, 1, 0, 26, 3]                                           │ sp:  4; fp:  3; ip: 11
0012: load   ~3     │ stack: [3, 1, 0, 26, 3, 3]                                        │ sp:  5; fp:  3; ip: 13
0014: iconst 1      │ stack: [3, 1, 0, 26, 3, 3, 1]                                     │ sp:  6; fp:  3; ip: 15
0016: isub          │ stack: [3, 1, 0, 26, 3, 2]                                        │ sp:  5; fp:  3; ip: 16
0017: call   0 1    │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19]                              │ sp:  8; fp:  8; ip: ~1
0000: load   ~3     │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2]                           │ sp:  9; fp:  8; ip:  1
0002: iconst 2      │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2, 2]                        │ sp: 10; fp:  8; ip:  3
0004: ilt           │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 0]                           │ sp:  9; fp:  8; ip:  4
0005: brf    10     │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19]                              │ sp:  8; fp:  8; ip:  9
0010: load   ~3     │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2]                           │ sp:  9; fp:  8; ip: 11
0012: load   ~3     │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2, 2]                        │ sp: 10; fp:  8; ip: 13
0014: iconst 1      │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2, 2, 1]                     │ sp: 11; fp:  8; ip: 15
0016: isub          │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2, 1]                        │ sp: 10; fp:  8; ip: 16
0017: call   0 1    │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2, 1, 1, 8, 19]              │ sp: 13; fp: 13; ip: ~1
0000: load   ~3     │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2, 1, 1, 8, 19, 1]           │ sp: 14; fp: 13; ip:  1
0002: iconst 2      │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2, 1, 1, 8, 19, 1, 2]        │ sp: 15; fp: 13; ip:  3
0004: ilt           │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2, 1, 1, 8, 19, 1]           │ sp: 14; fp: 13; ip:  4
0005: brf    10     │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2, 1, 1, 8, 19]              │ sp: 13; fp: 13; ip:  6
0007: iconst 1      │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2, 1, 1, 8, 19, 1]           │ sp: 14; fp: 13; ip:  8
0009: ret           │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2, 1]                        │ sp: 10; fp:  8; ip: 19
0020: imul          │ stack: [3, 1, 0, 26, 3, 2, 1, 3, 19, 2]                           │ sp:  9; fp:  8; ip: 20
0021: ret           │ stack: [3, 1, 0, 26, 3, 2]                                        │ sp:  5; fp:  3; ip: 19
0020: imul          │ stack: [3, 1, 0, 26, 6]                                           │ sp:  4; fp:  3; ip: 20
0021: ret           │ stack: [6]                                                        │ sp:  0; fp:  0; ip: 26
0027: print         │ stack: []                                                         │ sp: ~1; fp:  0; ip: 27
0028: halt          │ stack: []                                                         │ sp: ~1; fp:  0; ip: 28
6
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

[video]: https://www.youtube.com/watch?v=OjaAToVkoTw
[book]: https://pragprog.com/titles/tpdsl/language-implementation-patterns/
