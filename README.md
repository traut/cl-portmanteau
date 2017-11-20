# cl-portmanteau

*cl-portmanteau* is a library that makes a portmanteau of two words.

### Definition

A portmanteau ([from Wiki](https://en.wikipedia.org/wiki/Portmanteau)) - (/pɔːrtˈmæntoʊ/, /ˌpɔːrtmænˈtoʊ/[a]; plural portmanteaus or portmanteaux /-ˈtoʊz/[b]) or portmanteau word is a linguistic blend of words, in which parts of multiple words or their phones (sounds) are combined into a new word, as in _smog_, coined by blending _smoke_ and _fog_, or _motel_, from _motor_ and _hotel_. In linguistics, a portmanteau is defined as a single morph that represents two or more morphemes.

### Installation and usage
```lisp
CL-USER(1): (ql:quickload 'portmanteau)
To load "portmanteau":
  Load 1 ASDF system:
    portmanteau
; Loading "portmanteau"

(PORTMANTEAU)
CL-USER(2): (asdf:load-system 'portmanteau)

T
CL-USER(3): (portmanteau:portmanteau "mock" "documentary")

"mocumentary"
CL-USER(4):
```

### CLI
To run cl-portmanteau in CLI mode, you will need [Roswell](https://github.com/roswell/roswell) installed:

```bash
cl-portmanteau $ ros ./run.ros mock documentary
mocumentary
```

### Tests
```lisp
CL-USER(1): (asdf:load-system 'portmanteau)

T
CL-USER(2): (asdf:load-system 'portmanteau-tests)

T
CL-USER(3): (portmanteau-tests:all-tests)

Running test suite MAIN-SUITE
 Running test TOO-SHORT-TEST  <ERROR> [13:20:37] portmanteau - Both words must be longer than 3
.
 Running test MATCHING-TOO-CLOSE-TO-BEGINNING-TEST .
 Running test MATCHING-TOO-CLOSE-TO-END-TEST .
 Running test NOT-MATCHING-TEST .
 Running test MERGER-TEST ....
 Did 8 checks.
    Pass: 8 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
NIL
CL-USER(4):
```
or via Roswell:

```bash
cl-portmanteau $ ros ./run-tests.ros

Running test suite MAIN-SUITE
 Running test TOO-SHORT-TEST .
 Running test MATCHING-TOO-CLOSE-TO-BEGINNING-TEST .
 Running test MATCHING-TOO-CLOSE-TO-END-TEST .
 Running test NOT-MATCHING-TEST .
 Running test MERGER-TEST ....
 Did 8 checks.
    Pass: 8 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

cl-portmanteau $
```
