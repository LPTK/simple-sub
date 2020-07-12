# Simple Algebraic Subtyping

This repository shows the implementation of **Simple-sub**,
an alternative algorithm to MLsub for algebraic subtyping.

An online demo is available here: https://lptk.github.io/simple-sub/

The corresponding ICFP Pearl paper preprint can be downloaded here: https://lptk.github.io/simple-sub-paper


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
