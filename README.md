[![Stories in Ready](https://badge.waffle.io/brick-lang/kiln.png?label=ready)](https://waffle.io/brick-lang/kiln)  

# Kiln


Kiln is the reference compiler tool for the [Brick](https://github.com/brick-lang/brick-lang) programming language.  
It (will) include a compiler, a JIT interpreter, and a suite of extra programming tools.

Here's some of the commands:  
* `kiln fire` - compile the project, using the current directory's `Kilnfile` as reference
* `kiln glaze` - run the interactive top-level interpreter.
* `kiln test` - run a test suite with the test handler specified in the Kilnfile.
* `kiln doc` - compile all documentation from the project.
* `kiln check` - run a suite of linting and type-checking tools over the project. The scope of these tools can be specified in the Kilnfile.
* `kiln format` - run an automatic text formatting tool over the project, making sure that all source is compliant with the project's style specifications.
* `kiln run file` - run the file using the JIT interpreter

# Installation

First, [install OPAM and initialize it](http://opam.ocaml.org/doc/Quick_Install.html) if you haven't already.

```bash
opam pin add kiln . -n
opam install kiln
```

# Hacking
For in-source builds, there is a provided `Makefile` that will build to `_build`.

# Construction
We're adopting a front-to-back compiler construction model, albeit more incremental than most.
Right now, you can run `kiln parse path/to/file.br` and get a constructed AST from from well-formed input.

# Status
Current status is something like this:
- [x] Lexer
- [ ] Parser {partially constructed}
- [ ] Annotation (Typing, Parallel fragmentation, etc.)
  - Typing engine in progress, see [Kekka](https://github.com/brick-lang/kekka)
- [ ] Lambda IR Transformation (Similar to Haskell and OCaml)
- [ ] CMM Transformation
- [ ] LLVM IR emission
- [ ] Runtime (GC, primitives, parallel ops, etc.)
  - GC complete, see [Acid](https://github.com/brick-lang/acid)
