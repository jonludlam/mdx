The `var=NAME` label binds the raw contents of a block to an OCaml
string variable named `NAME`. The block itself is not executed, so
the label works on blocks of any language.

A text block:

```text var=greeting
hello,
world
```

```ocaml
# greeting;;
- : string = "hello,\nworld"
# String.length greeting;;
- : int = 12
```

An OCaml-headered block — header is only used for syntax highlighting;
contents are still bound verbatim:

```ocaml var=snippet
let x = "hi"
```

```ocaml
# snippet;;
- : string = "let x = \"hi\""
```

Combined with `env=`, the binding is scoped to that environment:

```ocaml env=other,var=snippet
scoped value
```

```ocaml env=other
# snippet;;
- : string = "scoped value"
```

The default env still has its own binding:

```ocaml
# snippet;;
- : string = "let x = \"hi\""
```
