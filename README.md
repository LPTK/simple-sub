# Simple Algebraic Subtyping

This repository shows the implementation of **Simple-sub**,
an alternative algorithm to MLsub for algebraic subtyping.

An online demo is available here: https://lptk.github.io/simple-sub/

The corresponding ICFP Pearl paper preprint can be downloaded here: https://lptk.github.io/simple-sub-paper


## ICFP 2020

This branch of the repository (called `mlsub-compare`)
contains the precise material corresponding to the ICFP 2020 paper mentioned above.

### Line number claims

In the paper, I claim that
"inferring MLsub types is surprisingly easy, and can be done in under 300 lines of Scala code,
with an additional 200 lines of code for simplification and pretty-printing".

The "less than 300 lines" claim refers to
the parsing, syntax definitions, type inference, and auxillary code in
`Parser.scala`, `Typer.scala`, `TyperHelpers.scala`, and `package.scala`.
These amount to 262 lines of code as measured by [the cloc tool](https://github.com/AlDanial/cloc) (which ignores comments and blank lines).  

The optional simplification and pretty-printing code
lines in `TypeSimplifier.scala` and `helpers.scala`,
which contain respectively 169 and 43 lines of code.

Thus, the total number of lines of code for all these functionalities
is 262 + 169 + 43 = 474 < 500.


## Running the tests

Running the tests only requires the Scala Build Tool installed.
In the terminal, run `sbt simplesubJVM/test`.


## Running the systematic testing

Make sure you have compiled `mlsub` first, which is in the `mlsub` git submodule.
The testing program is going to look for a `main.native` executable there.

To compile MLsub, one needs an installation of Opam (the OCaml package manager).
The required version of OCaml is 4.03.0; it can be obtained by running `opam switch create 4.03.0`.
Additional tools to install through opam are `menhir` and `ocamlbuild`
(install them with `opam install ocamlbuild menhir`).
Then, one has to run, in the `mlsub` subdirectory, `make main.native`,
which will create the `main.native` executable.

Then, to run the MLsub-comparison tests,
in the SBT shell of the main project type `simplesubJVM/test:run`.
This is going to use the `mlsub/main.native` executable to check the type inference results against MLsub.
It will print a summary of the tests in the console,
but all the test data will be output into a text file.

By default, only 799 expressions are tested
(the results will go into the `test_results_0.11-50.txt` file).
To enable the full tests (which will take a while), uncomment the following lines
in `jvm/src/test/scala/MLsubTests.scala`:
```scala
  // n == 1313832
  val StartP = 200
  val MultP = 0.11
  val fieldNames = "u" :: "v" :: "w" :: Nil
  val varNames = "x" :: "y" :: "z" :: "s" :: "t" :: Nil
```
The test results will then go to the `test_results_0.11-200.txt` file.

You can try commenting out some lines in the `Typer.scala`
or making other modifications,
and see the tests fail correspondingly.


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
