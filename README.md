# Simple-sub Algorithm for Algebraic Subtyping

This repository shows the implementation of **Simple-sub**,
an alternative algorithm to MLsub for type inference based on algebraic subtyping.

An online demo is available here: https://lptk.github.io/simple-sub/

The corresponding ICFP Pearl paper preprint can be downloaded here: https://lptk.github.io/simple-sub-paper


## Branches


### ICFP branch (for posterity)

The code which corresponds precisely to the ICFP paper mentioned above
can be found in the branch `mlsub-compare`,
which also contains instructions for compiling MLsub
and for systematically testing Simple-sub against it (on randomly-generated expressions).


### Master branch (current development)

The current `master` branch contains newer changes, including:
 
 * A type canonicalization algorithm to merge recursive types,
    so that for instance the type inferred for `let rec r = fun a -> r in if true then r else r`
    is just `(⊤ -> 'a) as 'a` instead of `⊤ -> (⊤ -> 'a) as 'a ∨ (⊤ -> 'b) as 'b`
    (see `[test:T2]` in the code).



## Running the tests

Running the tests only requires the Scala Build Tool installed.
In the terminal, run `sbt simplesubJVM/test`.


## Running the demo locally

To run the demo on your computer, first change the line in `index.html` from:
```html
<script type="text/javascript" src="bin/simple-algebraic-subtyping-opt.js"></script>
```
to:
```html
<script type="text/javascript" src="js/target/scala-2.13/simple-algebraic-subtyping-fastopt.js"></script>
```

And then compile the project with `sbt fastOptJS`.

Finally, open the file `index.html` in your browser.

You can make changes to the type inference code
in `shared/src/main/scala/simplesub`,
have it compile to JavaScript on file change with command
`sbt ~fastOptJS`,
and immediately see the results in your browser by refreshing the page with `F5`.
