[![Stories in Ready](https://badge.waffle.io/brick-lang/kiln.png?label=ready)](https://waffle.io/brick-lang/kiln)  

#Kiln


Kiln is the reference compiler tool for the [Brick](https://github.com/brick-lang/brick-lang) programming language.  
It includes a compiler, a JIT interpreter, and a suite of extra programming tools.

Here's some of the commands:  
* `kiln fire` - compile the project, using the current directory's `Kilnfile` as reference
* `kiln glaze` - run the interactive top-level interpreter. 
* `kiln test` - run a test suite with the test handler specified in the Kilnfile.
* `kiln doc` - compile all documentation from the project.
* `kiln check` - run a suite of linting and type-checking tools over the project. The scope of these tools can be specified in the Kilnfile.
* `kiln format` - run an automatic text formatting tool over the project, making sure that all source is compliant with the project's style specifications.
* `kiln run file` - run the file using the JIT interpreter

#Building the Kiln

First, [install OPAM and initialize it.](http://opam.ocaml.org/doc/Quick_Install.html)

```bash
opam install core async menhir oasis cfg res
oasis setup
ocaml setup.ml -configure
ocaml setup.ml -build
```

Note: This builds it in the current folder, and will not add it to your system path.
