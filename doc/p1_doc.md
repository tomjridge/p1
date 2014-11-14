
---
title: 'P1 documentation'
author: 'Tom Ridge'
date: 2014-11-11
bibliography: parsing.bib

---

# Meta

This documentation is stored on github in the `doc` subdirectory of
the `p1` repository (user `tomjridge`). It is intended to be formatted
to html using pandoc (see `Makefile` in `doc` dir). Periodically the
html version of this document is placed on the web.

  * <http://www.tom-ridge.com/parsing.html> - links to P1 documentation on the web

# Introduction

P1 is a combinator parsing library for OCaml. The main feature is:

  * P1 can parse arbitrary context-free grammars, including those with
    left recursion
    
However, in order to handle all context-free grammars, the interface
to P1 is slightly more complicated than that for standard combinator
parsers.

The theory behind P1 is explained in the paper [@ridge2011simple]. In the
following sections, I will explain informally how P1 can be used to
parse arbitrary context-free grammars.

# Other sources of information: P1 ocamldoc, P1 source code

The build directory Makefile contains a target to build ocamldoc
documentation for P1. This is a useful source of information. The
interface is defined in the `p1_lib.mli` file, and we include excerpts
from this file in the following sections.

  * <http://www.tom-ridge.com/resources/p1/ocamldoc/> - ocamldoc

In addition, the entire code for the core of P1 is under 40 lines. In
conjunction with [@ridge2011simple] it should be easy to understand
the functioning of P1 in its entirety. The following sections describe
the interface, for users who do not want to grapple with the details
of [@ridge2011simple].


# Building and running examples

  * In the `p1` top-level directory type `make` to build
  * Then to run some examples, type `build/p1_examples.native`
  * To run a code-generation example, change to the `gen` directory and type `make`

# Substrings

The API is defined in the file `p1_lib.mli`. 

P1 can be used as a lexerless parser (to parse strings directly), or
it can be used with a lexer. The input is typically represented by a
type parameter `'a`. We often work with parts of the input string,
rather than the entire input. Splitting strings into pieces
(i.e. smaller strings) is inefficient, so we introduce a type of
substrings.

```ocaml
type 'a substring = [ `SS of 'a * int * int ]
```

The idea is that a substring `` `SS(s,i,j)`` represents the part of
the input string `s` between indexes `i` and `j` (where $0 \le i \le j
\le |s|$). 


Parsers don't work directly with substrings, because they need to
maintain additional information about the parent parses that are
already in progress (this is tracked by the "parsing context", as
described in [@ridge2011simple]). For this reason, parsers work with a
type `'a ty_input` rather than `'a substring`. Effectively this type
contains a substring, and some "extra information" (the parsing
context). However, the user should never be exposed to the parsing
context. Hence, we only allow the user access to the substring
component.

```ocaml
type 'a ty_input
val toinput : 'a substring -> 'a ty_input
val substring_of_input : 'a ty_input -> 'a substring
```

# Terminal parsers and the type of parsers

In traditional parsing presentations, terminal parsers correspond to a
single character (or token) in the input. We lift this restriction,
and allow (more-or-less) arbitrary terminal parsers.

```ocaml
type ('a, 'b) ty_parser = 'a ty_input -> ('b * 'a substring) list
```

Here, `'a` is the underlying type of the input (typically `string`),
and `'b` is the type of result. As with traditional combinator
parsers, we are parsing prefixes of the input, and so the list of
results is a list of pairs, where each pair contains a value of type
`'b`, and a substring indicating the remainder of the input that has
still to be parsed.

A terminal parser can be constructed directly. For example, to parse a
single character `1` from the input, we do the following:

```ocaml
let a1 : (string,int) ty_parser = (fun i0 ->
    let `SS(s,i,j) = substring_of_input i0 in
    if i < j && s.[i] = '1' then 
      [(1,`SS(s,i+1,j))]
    else
      [])
```

The library provides some standard terminal parsers. For example, the
above can be written as `(a "1") >> (fun _ -> 1)`, where `a` is a function that takes
a string and returns a parser that consumes exactly that string.


# Combinators and the semantic action function

With non-left-recursive grammars, P1 functions like a traditional
combinator parsing library. There are three main combinators. The
sequential combinator `**>` (written infix) sequences two parsers. The
following is simple OCaml code to define the parser `p` as the parser
`p1` followed by `p2`:

```ocaml 
let p = p1 **> p2 
```

The alternative combination `p1 ||| p2` returns the results of parsing
with `p1` and `p2`. In fact, it is defined using simple list
concatenation, exactly as traditional combinator parsers.

Finally, the "semantic action" function `p >> f` applies a function
`f` to each of the results of parsing with `p`.

The API gives the following types:

```ocaml
type ('a, 'b) ty_parser = 'a ty_input -> ('b * 'a substring) list

val ( >> ) : ('a,'b) ty_parser -> ('b -> 'c) -> ('a,'c) ty_parser
val ( ||| ) : ('a,'b) ty_parser -> ('a,'b) ty_parser -> ('a,'b) ty_parser
val ( **> ) : ('a,'b) ty_parser -> ('a,'c) ty_parser -> ('a, 'b*'c) ty_parser
```

# Left-recursive grammars

P1 handles left-recursive grammars by tracking parent parses that are
already in progress. For example, suppose we are parsing a nonterminal
`E` for the substring $S_{i,j}$. If there is a parent parse in
progress for the same nonterminal, for the same substring, then the
parse should be abandoned [^za2].

[^za2]: The paper [@ridge2011simple] makes it clear why this is a
semantically reasonable thing to do. The formal definition involves
the notion of "good parse tree". The P1 parsing strategy returns all
good parse trees, and these are essentially all the parse trees you
are usually interested in. Semantically, you can typically reason as
though you were working with all parse trees, and provided your
parsing actions are "reasonable" (in a technical sense), after
applying the semantic actions, you get all the results you expect.


# Memoization


# References
